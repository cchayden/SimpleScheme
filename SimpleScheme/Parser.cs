// <copyright file="Parser.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Parse scheme expressions.
    /// </summary>
    public class Parser
    {
        #region Fields
        /// <summary>
        /// The character stream to read from.
        /// </summary>
        private readonly CharacterStream charStream = new CharacterStream();

        /// <summary>
        /// The token stream to read from.
        /// </summary>
        private readonly TokenStream tokStream = new TokenStream();

        /// <summary>
        /// The TextReader we are reading from to get the input.
        /// </summary>
        private readonly TextReader inp;

        /// <summary>
        /// Used to make a transcript of the input.
        /// </summary>
        private StringBuilder logger;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Parser class.
        /// </summary>
        /// <param name="inp">The input TextReader we are reading from.</param>
        public Parser(TextReader inp)
        {
            this.inp = inp;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Read a complete expression.
        /// </summary>
        /// <param name="sb">The characters read are recorded in this StringBuilder.</param>
        /// <returns>The expression that was read.</returns>
        public Obj ReadExpr(StringBuilder sb)
        {
            this.logger = sb;
            return this.Read();
        }

        /// <summary>
        /// Take a peek at the next character, without consuming it.
        /// </summary>
        /// <returns>The next character (as a scheme character).</returns>
        public Obj PeekChar()
        {
            int p = this.Peek();
            if (p == -1)
            {
                return InputPort.Eof;
            }

            return Character.As((char)p);
        }

        /// <summary>
        /// Read a character from the input port.
        /// Gets a pushed character, if present.
        /// </summary>
        /// <param name="sb">The characters read are recorded in this StringBuilder.</param>
        /// <returns>The character read, or EOF.</returns>
        public object ReadChar(StringBuilder sb)
        {
            this.logger = sb;
            try
            {
                int ch = this.charStream.Get();
                if (ch == -2)
                {
                    ch = this.ReadNextChar();
                }

                if (ch == -1)
                {
                    return InputPort.Eof;
                }

                return Character.As((char)ch);
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception: " + ex);
                return InputPort.Eof;
            }
        }

        /// <summary>
        /// Close the input port.
        /// </summary>
        public void Close()
        {
            try
            {
                this.inp.Close();
            }
            catch (IOException ex)
            {
                ErrorHandlers.IoError("IOException on close: " + ex);
            }
        }

        /// <summary>
        /// Gets the next token from the input stream.
        /// Gets a pushed token if there is one, otherwise reads from the input.
        /// </summary>
        /// <returns>The next token.</returns>
        public object NextToken()
        {
            // See if we should re-use a pushed token or character
            object token = this.tokStream.GetPushedToken();
            if (token != null)
            {
                return token;
            }

            int ch = this.ReadOrPop();

            // Skip whitespace
            while (char.IsWhiteSpace((char)ch))
            {
                ch = this.ReadNextChar();
            }

            // See what kind of non-whitespace character we got
            switch (ch)
            {
                case -1:
                    return InputPort.Eof;

                case '(':
                    return "(";

                case ')':
                    return ")";

                case '\'':
                    return "'";

                case '`':
                    return "`";

                case ',':
                    ch = this.ReadNextChar();
                    if (ch == '@')
                    {
                        return ",@";
                    }

                    this.charStream.Push(ch);
                    return ",";

                case ';':
                    while (ch != -1 && ch != '\n' && ch != '\r')
                    {
                        ch = this.ReadNextChar();
                    }

                    return this.NextToken();

                case '"':
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        while ((ch = this.ReadNextChar()) != '"' & ch != -1)
                        {
                            buff.Append((char)((ch == '\\') ? this.ReadNextChar() : ch));
                        }

                        if (ch == -1)
                        {
                            ErrorHandlers.Warn("EOF inside of a string.");
                        }

                        return SchemeString.New(buff);
                    }

                case '#':
                    switch (ch = this.ReadNextChar())
                    {
                        case 't':
                        case 'T':
                            return SchemeBoolean.True;

                        case 'f':
                        case 'F':
                            return SchemeBoolean.False;

                        case '(':
                            this.charStream.Push('(');
                            return Vector.FromList(this.Read());

                        case '\\':
                            ch = this.ReadNextChar();
                            if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N')
                            {
                                this.charStream.Push(ch);
                                token = this.NextToken();
                                if (token is string && (token as string).Length == 1)
                                {
                                    return Character.As((char)ch);
                                }

                                switch (token as string)
                                {
                                    case "space":
                                        return Character.As(' ');
                                    case "newline":
                                        return Character.As('\n');
                                    default:
                                        // this isn't really right
                                        // #\<char> is required to have delimiter after char
                                        ErrorHandlers.Warn("#\\<char> must be followed by delimiter");
                                        this.tokStream.PushToken(token);
                                        return Character.As((char)ch);
                                }
                            }

                            return Character.As((char)ch);

                        case 'e':
                        case 'i':
                        case 'd':
                            return this.NextToken();

                        case 'b':
                        case 'o':
                        case 'x':
                            ErrorHandlers.Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken();

                        default:
                            ErrorHandlers.Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken();
                    }

                default:
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        int c = ch;
                        do
                        {
                            buff.Append((char)ch);
                            ch = this.ReadNextChar();
                        } 
                        while (!char.IsWhiteSpace((char)ch) && ch != -1 && 
                            ch != '(' && ch != ')' && ch != '\'' &&
                                 ch != ';' && ch != '"' && ch != ',' && ch != '`');

                        this.charStream.Push(ch);
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

        #endregion

        #region Private Methods
        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        private Obj Read()
        {
            try
            {
                object token = this.NextToken();

                switch (token as string)
                {
                    case "(":
                        token = this.ReadTail(false);
                        break;

                    case ")":
                        ErrorHandlers.Warn("Extra ) ignored.");
                        token = this.Read();
                        break;
                    case ".":
                        ErrorHandlers.Warn("Extra . ignored.");
                        token = this.Read();
                        break;
                    case "'": 
                        token = List.New("quote", this.Read());
                        break;
                    case "`":
                        token = List.New("quasiquote", this.Read());
                        break;
                    case ",": 
                        token = List.New("unquote", this.Read());
                        break;
                    case ",@": 
                        token = List.New("unquote-splicing", this.Read());
                        break;
                    default:
                        break;
                }

                return token;
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception:" + ex);
                return InputPort.Eof;
            }
        }

        /// <summary>
        /// Peek into the input stream, and return the next character to be read.
        /// The character is saved, so that the next read will see it.
        /// If the input stream is closed, return -1.
        /// </summary>
        /// <returns>The next character in the stream.</returns>
        private int Peek()
        {
            int ch = this.charStream.Peek();
            if (ch != -1)
            {
                return ch;
            }

            try
            {
                this.charStream.Push(ch = this.inp.Read());
                return ch;
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception: " + ex);
                return -1;
            }
        }

        /// <summary>
        /// Pop a buffered character, if one is availab.e
        /// Otherwise read one from the stream.
        /// </summary>
        /// <returns>The character read.</returns>
        private int ReadOrPop()
        {
            int ch = this.charStream.Pop();
            if (ch != -1)
            {
                return ch;
            }

            ch = this.inp.Read();

            if (this.logger != null)
            {
                this.logger.Append((char)ch);
            }

            return ch;
        }

        /// <summary>
        /// Read the next character from the reader.
        /// If there is a buffered character, throw it away and give a warning.
        /// </summary>
        /// <returns>The character read.</returns>
        private int ReadNextChar()
        {
            int ch = this.charStream.Pop();
            if (ch != -1)
            {
                    ErrorHandlers.IoError("Read bypassed pushed char.");
            }

            ch = this.inp.Read();
            if (this.logger != null)
            {
                this.logger.Append((char)ch);
            }

            return ch;
        }

        /// <summary>
        /// Read the tail of a list.
        /// The opening left paren has been read, also perhaps some of the list.
        /// </summary>
        /// <param name="dotOk">True if a dot is OK at this point.</param>
        /// <returns>A list of the tokens read.</returns>
        private Obj ReadTail(bool dotOk)
        {
            object token = this.NextToken();
            if (token as string == InputPort.Eof)
            {
                return ErrorHandlers.IoError("EOF during read.");
            }

            if (token as string == ")")
            {
                return EmptyList.Instance;  // there was no more
            }

            if (token as string == ".")
            {
                if (!dotOk)
                {
                    ErrorHandlers.Warn("Dot not allowed here, ignored.");
                    return this.ReadTail(false);
                }

                object result = this.Read();
                token = this.NextToken();
                if (token as string != ")")
                {
                    ErrorHandlers.Warn("Expecting ')' got " + token + " after dot");
                }

                return result;
            }

            this.tokStream.PushToken(token);
            return Pair.Cons(this.Read(), this.ReadTail(true));
        }
        #endregion

        #region Private Classes
        /// <summary>
        /// Manages a character stream where it is possible to peek ahead one
        ///   character, or to push back a character that was already read.
        /// </summary>
        private class CharacterStream
        {
            /// <summary>
            /// True if there is a pushed character.
            /// </summary>
            private bool isPushedChar; // = false;

            /// <summary>
            /// If there is a pushed character, this is it.
            /// </summary>
            private int pushedChar = -1;

            /// <summary>
            /// Push a character back into the input.
            /// </summary>
            /// <param name="ch">The character to push.</param>
            public void Push(int ch)
            {
                this.isPushedChar = true;
                this.pushedChar = ch;
            }

            /// <summary>
            /// Get a pushed char, if present, or else return -1.
            /// </summary>
            /// <returns>The pushed character.</returns>
            public int Pop()
            {
                if (this.isPushedChar)
                {
                    this.isPushedChar = false;
                    return this.pushedChar;
                }

                return -1;
            }

            /// <summary>
            /// Check to see if there is a pushed character.
            /// If so, return it.
            /// </summary>
            /// <returns>The pushed character, or -1 if EOF, or -2 if there is no pushed character.</returns>
            public int Get()
            {
                if (this.isPushedChar)
                {
                    this.isPushedChar = false;
                    if (this.pushedChar == -1)
                    {
                        return -1;
                    }

                    return Character.As((char)this.pushedChar);
                }

                return -2;
            }

            /// <summary>
            /// Take a peek at the next character, without consuming it.
            /// If there is none, return -1.
            /// </summary>
            /// <returns>The next character (as an int).</returns>
            public int Peek()
            {
                return this.isPushedChar ? this.pushedChar : -1;
            }
        }

        /// <summary>
        /// Handles tokens that have been read but pushed back on the input stream.
        /// </summary>
        private class TokenStream
        {
            /// <summary>
            /// True if there is a pushed token.
            /// </summary>
            private bool isPushedToken; // = false;

            /// <summary>
            /// If there is a pushed token, this is it.
            /// </summary>
            private object pushedToken;

            /// <summary>
            /// Push the token back on the input.
            /// </summary>
            /// <param name="token">The token to push.</param>
            public void PushToken(object token)
            {
                this.isPushedToken = true;
                this.pushedToken = token;
            }

            /// <summary>
            /// Get the pushed token, if available.
            /// If not, return null.
            /// </summary>
            /// <returns>The pushed token, if available, otherwise null.</returns>
            public object GetPushedToken()
            {
                return this.isPushedToken ? this.PopToken() : null;
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
        }
        #endregion
    }
}
