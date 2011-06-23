// <copyright file="InputPort.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;

    /// <summary>
    /// Represents an input port, a mechanism for reading input.
    /// </summary>
    public sealed class InputPort : SchemeUtils
    {
        /// <summary>
        /// Marks the end of the input file.
        /// </summary>
        public const string Eof = "#!EOF";

        /// <summary>
        /// The input that is being read.
        /// </summary>
        private readonly TextReader inp;

        /// <summary>
        /// True if there is a pushed token.
        /// </summary>
        private bool isPushedToken; // = false;

        /// <summary>
        /// If there is a pushed token, this is it.
        /// </summary>
        private object pushedToken;

        /// <summary>
        /// True if there is a pushed character.
        /// </summary>
        private bool isPushedChar; // = false;

        /// <summary>
        /// If there is a pushed character, this is it.
        /// </summary>
        private int pushedChar = -1;

        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// </summary>
        /// <param name="inp">A text reader</param>
        public InputPort(TextReader inp)
        {
            this.inp = inp;
        }

        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// </summary>
        /// <param name="inp">An input stream.</param>
        public InputPort(Stream inp)
            : this(new StreamReader(inp))
        {
        }

        /// <summary>
        /// Tests the object against EOF.
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if the object is EOF.</returns>
        public static bool IsEOF(object x)
        {
            return x as string == Eof;
        }

        /// <summary>
        /// Close the input port.
        /// </summary>
        /// <returns>True if the port was closed.</returns>
        public object Close()
        {
            try
            {
                this.inp.Close();
                return True;
            }
            catch (IOException ex)
            {
                return Error("IOException: " + ex);
            }
        }

        /// <summary>
        /// Read a character from the input port.
        /// Gets a pushed character, if present.
        /// </summary>
        /// <returns>The character read, or EOF.</returns>
        public object ReadChar()
        {
            try
            {
                if (this.isPushedChar)
                {
                    this.isPushedChar = false;
                    if (this.pushedChar == -1)
                    {
                        return Eof;
                    }

                    return Chr((char)this.pushedChar);
                }

                int ch = this.inp.Read();
                if (ch == -1)
                {
                    return Eof;
                }

                return Chr((char)ch);
            }
            catch (IOException ex)
            {
                Warn("On input, exception: " + ex);
                return Eof;
            }
        }

        /// <summary>
        /// Take a peek at the next character, without consuming it.
        /// Either read and save a character, to take a look at an already saved character.
        /// </summary>
        /// <returns>The next character (as a character).</returns>
        public int PeekCh()
        {
            try
            {
                return this.isPushedChar ? this.pushedChar : this.PushChar(this.inp.Read());
            }
            catch (IOException ex)
            {
                Warn("On input, exception: " + ex);
                return -1;
            }
        }

        /// <summary>
        /// Take a peek at the next character, without consuming it.
        /// </summary>
        /// <returns>The next character (as a cell).</returns>
        public object PeekChar()
        {
            int p = this.PeekCh();
            if (p == -1)
            {
                return Eof;
            }

            return Chr((char)p);
        }

        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        public object Read()
        {
            try
            {
                object token = this.NextToken();

                switch (token as string)
                {
                    case "(": 
                        return this.ReadTail(false);
                    case ")":
                        Warn("Extra ) ignored.");
                        return this.Read();
                    case ".":
                        Warn("Extra . ignored.");
                        return this.Read();
                    case "'": 
                        return List("quote", this.Read());
                    case "`":
                        return List("quasiquote", this.Read());
                    case ",": 
                        return List("unquote", this.Read());
                    case ",@": 
                        return List("unquote-splicing", this.Read());
                    default:
                        return token;
                }
            }
            catch (IOException ex)
            {
                Warn("On input, exception:" + ex);
                return Eof;
            }
        }

        /// <summary>
        /// Gets the next token from the input port.
        /// Gets a pushed token if there is one, otherwise reads from the input.
        /// </summary>
        /// <returns>the next token.</returns>
        private object NextToken()
        {
            // See if we should re-use a pushed token or character
            if (this.isPushedToken)
            {
                return this.PopToken();
            }

            int ch = this.isPushedChar ? this.PopChar() : this.inp.Read();

            // Skip whitespace
            while (char.IsWhiteSpace((char)ch))
            {
                ch = this.inp.Read();
            }

            // See what kind of non-whitespace character we got
            switch (ch)
            {
                case -1:
                    return Eof;

                case '(':
                    return "(";

                case ')':
                    return ")";

                case '\'':
                    return "'";

                case '`':
                    return "`";

                case ',':
                    ch = this.inp.Read();
                    if (ch == '@')
                    {
                        return ",@";
                    }

                    this.PushChar(ch);
                    return ",";

                case ';':
                    while (ch != -1 && ch != '\n' && ch != '\r')
                    {
                        ch = this.inp.Read();
                    }

                    return this.NextToken();

                case '"':
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        while ((ch = this.inp.Read()) != '"' & ch != -1)
                        {
                            buff.Append((char)((ch == '\\') ? this.inp.Read() : ch));
                        }

                        if (ch == -1)
                        {
                            Warn("EOF inside of a string.");
                        }

                        return buff.ToString().ToCharArray();
                    }

                case '#':
                    switch (ch = this.inp.Read())
                    {
                        case 't':
                        case 'T':
                            return True;

                        case 'f':
                        case 'F':
                            return False;

                        case '(':
                            this.PushChar('(');
                            return ListToVector(this.Read());

                        case '\\':
                            ch = this.inp.Read();
                            if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N')
                            {
                                this.PushChar(ch);
                                object token = this.NextToken();
                                if (token is string && (token as string).Length == 1)
                                {
                                    return Chr((char)ch);
                                }

                                switch (token as string)
                                {
                                    case "space":
                                        return Chr(' ');
                                    case "newline":
                                        return Chr('\n');
                                    default:
                                        // this isn't really right
                                        // #\stop return 's' and then "stop"
                                        // #\<char> is required to have delimiter after char
                                        Warn("#\\<char> bus be followed by delimiter");
                                        this.PushToken(token);
                                        return Chr((char)ch);
                                }
                            }

                            return Chr((char)ch);

                        case 'e':
                        case 'i':
                        case 'd':
                            return this.NextToken();

                        case 'b':
                        case 'o':
                        case 'x':
                            Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken();

                        default:
                            Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken();
                    }

                default:
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        int c = ch;
                        do
                        {
                            buff.Append((char)ch);
                            ch = this.inp.Read();
                        } 
                        while (!char.IsWhiteSpace((char)ch) && ch != -1 && 
                            ch != '(' && ch != ')' && ch != '\'' &&
                                 ch != ';' && ch != '"' && ch != ',' && ch != '`');

                        this.PushChar(ch);
                        if (c == '.' || c == '+' || c == '-' || (c >= '0' && c <= '9'))
                        {
                            double value;
                            if (double.TryParse(buff.ToString(), out value))
                            {
                                return value;
                            }
                        }

                        return string.Intern(buff.ToString().ToLower());
                    }
            }
        }

        /// <summary>
        /// Push a character back into the input.
        /// </summary>
        /// <param name="ch">The character to push.</param>
        /// <returns>The character that was pushed.</returns>
        private int PushChar(int ch)
        {
            this.isPushedChar = true;
            this.pushedChar = ch;
            return ch;
        }

        /// <summary>
        /// Get the character that had been pushed.
        /// </summary>
        /// <returns>The character that was pushed.</returns>
        private int PopChar()
        {
            this.isPushedChar = false;
            return this.pushedChar;
        }

        /// <summary>
        /// Push the token back on the input.
        /// </summary>
        /// <param name="token">The token to push.</param>
        private void PushToken(object token)
        {
            this.isPushedToken = true;
            this.pushedToken = token;
        }

        /// <summary>
        /// Get the token that had been pushed.
        /// </summary>
        /// <returns>The pushed token.</returns>
        private object PopToken()
        {
            this.isPushedToken = false;
            return this.pushedToken;
        }

        /// <summary>
        /// Read the tail of a list.
        /// The opening left paren has been read, also perhaps some of the list.
        /// </summary>
        /// <param name="dotOK">True if a dot is OK at this point.</param>
        /// <returns>A list of the tokens read.</returns>
        private object ReadTail(bool dotOK)
        {
            object token = this.NextToken();
            if (token as string == Eof)
            {
                return Error("EOF during read.");
            }

            if (token as string == ")")
            {
                return null;  // there was no more
            }

            if (token as string == ".")
            {
                if (! dotOK)
                {
                    Warn("Dot not allowed here, ignored.");
                    return this.ReadTail(false);
                }

                object result = this.Read();
                token = this.NextToken();
                if (token as string != ")")
                {
                    Warn("Where's the ')'? Got " + token + " after .");
                }

                return result;
            }

            this.PushToken(token);
            return Cons(this.Read(), this.ReadTail(true));
        }
    }
}