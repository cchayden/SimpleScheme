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
    internal class Parser
    {
        #region Fields
        /// <summary>
        /// The character stream to read from.
        /// </summary>
        private readonly CharacterStream inStream;

        /// <summary>
        /// The token stream to read from.
        /// </summary>
        private readonly TokenStream tokStream;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Parser class.
        /// </summary>
        /// <param name="inp">The input (a TextReader).</param>
        public Parser(TextReader inp)
        {
            this.inStream = new CharacterStream(inp);
            this.tokStream = new TokenStream();
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Close the input port.
        /// </summary>
        internal void Close()
        {
            this.inStream.Close();
        }

        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        internal Obj Read()
        {
            try
            {
                object token = this.NextToken();

                switch (token as string)
                {
                    case "(": 
                        return this.ReadTail(false);
                    case ")":
                        ErrorHandlers.Warn("Extra ) ignored.");
                        return this.Read();
                    case ".":
                        ErrorHandlers.Warn("Extra . ignored.");
                        return this.Read();
                    case "'": 
                        return ListPrimitives.MakeList("quote", this.Read());
                    case "`":
                        return ListPrimitives.MakeList("quasiquote", this.Read());
                    case ",": 
                        return ListPrimitives.MakeList("unquote", this.Read());
                    case ",@": 
                        return ListPrimitives.MakeList("unquote-splicing", this.Read());
                    default:
                        return token;
                }
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception:" + ex);
                return InputPort.Eof;
            }
        }

        /// <summary>
        /// Take a peek at the next character, without consuming it.
        /// </summary>
        /// <returns>The next character (as a cell).</returns>
        internal object PeekChar()
        {
            int p = this.inStream.PeekCh();
            if (p == -1)
            {
                return InputPort.Eof;
            }

            return Character.Chr((char)p);
        }

        /// <summary>
        /// Read a character from the input port.
        /// Gets a pushed character, if present.
        /// </summary>
        /// <returns>The character read, or EOF.</returns>
        internal object ReadChar()
        {
            try
            {
                int ch = this.inStream.GetPushedChar();
                if (ch == -2)
                {
                    ch = this.inStream.ReadChar();
                }

                if (ch == -1)
                {
                    return InputPort.Eof;
                }

                return Character.Chr((char)ch);
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception: " + ex);
                return InputPort.Eof;
            }
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Gets the next token from the input port.
        /// Gets a pushed token if there is one, otherwise reads from the input.
        /// </summary>
        /// <returns>the next token.</returns>
        private object NextToken()
        {
            // See if we should re-use a pushed token or character
            object token = this.tokStream.GetPushedToken();
            if (token != null)
            {
                return token;
            }

            int ch = this.inStream.ReadOrPop();

            // Skip whitespace
            while (char.IsWhiteSpace((char)ch))
            {
                ch = this.inStream.ReadChar();
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
                    ch = this.inStream.ReadChar();
                    if (ch == '@')
                    {
                        return ",@";
                    }

                    this.inStream.PushChar(ch);
                    return ",";

                case ';':
                    while (ch != -1 && ch != '\n' && ch != '\r')
                    {
                        ch = this.inStream.ReadChar();
                    }

                    return this.NextToken();

                case '"':
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        while ((ch = this.inStream.ReadChar()) != '"' & ch != -1)
                        {
                            buff.Append((char)((ch == '\\') ? this.inStream.ReadChar() : ch));
                        }

                        if (ch == -1)
                        {
                            ErrorHandlers.Warn("EOF inside of a string.");
                        }

                        return SchemeString.MakeString(buff);
                    }

                case '#':
                    switch (ch = this.inStream.ReadChar())
                    {
                        case 't':
                        case 'T':
                            return SchemeBoolean.True;

                        case 'f':
                        case 'F':
                            return SchemeBoolean.False;

                        case '(':
                            this.inStream.PushChar('(');
                            return Vector.New(this.Read());

                        case '\\':
                            ch = this.inStream.ReadChar();
                            if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N')
                            {
                                this.inStream.PushChar(ch);
                                token = this.NextToken();
                                if (token is string && (token as string).Length == 1)
                                {
                                    return Character.Chr((char)ch);
                                }

                                switch (token as string)
                                {
                                    case "space":
                                        return Character.Chr(' ');
                                    case "newline":
                                        return Character.Chr('\n');
                                    default:
                                        // this isn't really right
                                        // #\<char> is required to have delimiter after char
                                        ErrorHandlers.Warn("#\\<char> must be followed by delimiter");
                                        this.tokStream.PushToken(token);
                                        return Character.Chr((char)ch);
                                }
                            }

                            return Character.Chr((char)ch);

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
                            ch = this.inStream.ReadChar();
                        } 
                        while (!char.IsWhiteSpace((char)ch) && ch != -1 && 
                            ch != '(' && ch != ')' && ch != '\'' &&
                                 ch != ';' && ch != '"' && ch != ',' && ch != '`');

                        this.inStream.PushChar(ch);
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
                return List.Empty;  // there was no more
            }

            if (token as string == ".")
            {
                if (! dotOk)
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
            return ListPrimitives.Cons(this.Read(), this.ReadTail(true));
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
            /// The input port to read from.
            /// </summary>
            private readonly TextReader inp;

            /// <summary>
            /// True if there is a pushed character.
            /// </summary>
            private bool isPushedChar; // = false;

            /// <summary>
            /// If there is a pushed character, this is it.
            /// </summary>
            private int pushedChar = -1;

            /// <summary>
            /// Initializes a new instance of the Parser.CharacterStream class.
            /// </summary>
            /// <param name="inp">The input port to use for reading.</param>
            internal CharacterStream(TextReader inp)
            {
                this.inp = inp;
            }

            /// <summary>
            /// Read a character from the input, bypassing the pushed char.
            /// </summary>
            /// <returns>The next character in the stream.</returns>
            internal int ReadChar()
            {
                if (this.isPushedChar)
                {
                    ErrorHandlers.IoError("Read bypassed pushed char.");
                    this.isPushedChar = false;
                }

                return this.inp.Read();
            }

            /// <summary>
            /// Push a character back into the input.
            /// </summary>
            /// <param name="ch">The character to push.</param>
            /// <returns>The character that was pushed.</returns>
            internal int PushChar(int ch)
            {
                this.isPushedChar = true;
                this.pushedChar = ch;
                return ch;
            }

            /// <summary>
            /// Get a pushed char, if present, or else read one.
            /// </summary>
            /// <returns>The next character in the stream.</returns>
            internal int ReadOrPop()
            {
                return this.isPushedChar ? this.PopChar() : this.inp.Read();
            }

            /// <summary>
            /// Check to see if there is a pushed character.
            /// If so, return it.
            /// </summary>
            /// <returns>The pushed character, or -1 if EOF, or -2 if there is no pushed character.</returns>
            internal int GetPushedChar()
            {
                if (this.isPushedChar)
                {
                    this.isPushedChar = false;
                    if (this.pushedChar == -1)
                    {
                        return -1;
                    }

                    return Character.Chr((char)this.pushedChar);
                }

                return -2;
            }

            /// <summary>
            /// Take a peek at the next character, without consuming it.
            /// Either read and save a character, to take a look at an already saved character.
            /// </summary>
            /// <returns>The next character (as a character).</returns>
            internal int PeekCh()
            {
                try
                {
                    return this.isPushedChar ? this.pushedChar : this.PushChar(this.inp.Read());
                }
                catch (IOException ex)
                {
                    ErrorHandlers.Warn("On input, exception: " + ex);
                    return -1;
                }
            }

            /// <summary>
            /// Close the input port.
            /// </summary>
            internal void Close()
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
            /// Get the character that had been pushed.
            /// </summary>
            /// <returns>The character that was pushed.</returns>
            private int PopChar()
            {
                this.isPushedChar = false;
                return this.pushedChar;
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
            internal void PushToken(object token)
            {
                this.isPushedToken = true;
                this.pushedToken = token;
            }

            /// <summary>
            /// Get the pushed token, if available.
            /// If not, return null.
            /// </summary>
            /// <returns>The pushed token, if available, otherwise null.</returns>
            internal object GetPushedToken()
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
