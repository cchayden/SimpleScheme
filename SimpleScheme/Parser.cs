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
        private readonly CharacterStream charStream = new CharacterStream();

        /// <summary>
        /// The token stream to read from.
        /// </summary>
        private readonly TokenStream tokStream = new TokenStream();
        #endregion

        #region Internal Methods
        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        internal Obj Read(TextReader inp)
        {
            try
            {
                object token = this.NextToken(inp);

                switch (token as string)
                {
                    case "(": 
                        return this.ReadTail(inp, false);
                    case ")":
                        ErrorHandlers.Warn("Extra ) ignored.");
                        return this.Read(inp);
                    case ".":
                        ErrorHandlers.Warn("Extra . ignored.");
                        return this.Read(inp);
                    case "'": 
                        return List.New("quote", this.Read(inp));
                    case "`":
                        return List.New("quasiquote", this.Read(inp));
                    case ",": 
                        return List.New("unquote", this.Read(inp));
                    case ",@": 
                        return List.New("unquote-splicing", this.Read(inp));
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
        /// <returns>The next character (as a scheme character).</returns>
        internal Obj PeekChar(TextReader inp)
        {
            int p = this.PeekCh(inp);
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
        internal object ReadChar(TextReader inp)
        {
            try
            {
                int ch = this.charStream.Get();
                if (ch == -2)
                {
                    ch = this.ReadNextChar(inp);
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
        private int PeekCh(TextReader inp)
        {
            int ch = this.charStream.Peek();
            if (ch != -1)
            {
                return ch;
            }

            try
            {
                this.charStream.Push(ch = inp.Read());
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
        /// <param name="inp">The input stream to read from.</param>
        /// <returns>The character read.</returns>
        private int ReadOrPop(TextReader inp)
        {
            int ch = this.charStream.Pop();
            if (ch != -1)
            {
                return ch;
            }

            return inp.Read();
        }

        /// <summary>
        /// Read the next character from the reader.
        /// If there is a buffered character, throw it away and give a warning.
        /// </summary>
        /// <param name="inp">The input stream to read from.</param>
        /// <returns>The character read.</returns>
        private int ReadNextChar(TextReader inp)
        {
            int ch = this.charStream.Pop();
            if (ch != -1)
            {
                    ErrorHandlers.IoError("Read bypassed pushed char.");
            }

            return inp.Read();
        }

        /// <summary>
        /// Gets the next token from the input stream.
        /// Gets a pushed token if there is one, otherwise reads from the input.
        /// </summary>
        /// <returns>the next token.</returns>
        private object NextToken(TextReader inp)
        {
            // See if we should re-use a pushed token or character
            object token = this.tokStream.GetPushedToken();
            if (token != null)
            {
                return token;
            }

            int ch = this.ReadOrPop(inp);

            // Skip whitespace
            while (char.IsWhiteSpace((char)ch))
            {
                ch = this.ReadNextChar(inp);
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
                    ch = this.ReadNextChar(inp);
                    if (ch == '@')
                    {
                        return ",@";
                    }

                    this.charStream.Push(ch);
                    return ",";

                case ';':
                    while (ch != -1 && ch != '\n' && ch != '\r')
                    {
                        ch = this.ReadNextChar(inp);
                    }

                    return this.NextToken(inp);

                case '"':
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        while ((ch = this.ReadNextChar(inp)) != '"' & ch != -1)
                        {
                            buff.Append((char)((ch == '\\') ? this.ReadNextChar(inp) : ch));
                        }

                        if (ch == -1)
                        {
                            ErrorHandlers.Warn("EOF inside of a string.");
                        }

                        return SchemeString.MakeString(buff);
                    }

                case '#':
                    switch (ch = this.ReadNextChar(inp))
                    {
                        case 't':
                        case 'T':
                            return SchemeBoolean.True;

                        case 'f':
                        case 'F':
                            return SchemeBoolean.False;

                        case '(':
                            this.charStream.Push('(');
                            return Vector.FromList(this.Read(inp));

                        case '\\':
                            ch = this.ReadNextChar(inp);
                            if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N')
                            {
                                this.charStream.Push(ch);
                                token = this.NextToken(inp);
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
                            return this.NextToken(inp);

                        case 'b':
                        case 'o':
                        case 'x':
                            ErrorHandlers.Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken(inp);

                        default:
                            ErrorHandlers.Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken(inp);
                    }

                default:
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        int c = ch;
                        do
                        {
                            buff.Append((char)ch);
                            ch = this.ReadNextChar(inp);
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

        /// <summary>
        /// Read the tail of a list.
        /// The opening left paren has been read, also perhaps some of the list.
        /// </summary>
        /// <param name="inp">The input stream.</param>
        /// <param name="dotOk">True if a dot is OK at this point.</param>
        /// <returns>A list of the tokens read.</returns>
        private Obj ReadTail(TextReader inp, bool dotOk)
        {
            object token = this.NextToken(inp);
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
                    return this.ReadTail(inp, false);
                }

                object result = this.Read(inp);
                token = this.NextToken(inp);
                if (token as string != ")")
                {
                    ErrorHandlers.Warn("Expecting ')' got " + token + " after dot");
                }

                return result;
            }

            this.tokStream.PushToken(token);
            return List.Cons(this.Read(inp), this.ReadTail(inp, true));
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
            /// <returns>The character that was pushed.</returns>
            internal void Push(int ch)
            {
                this.isPushedChar = true;
                this.pushedChar = ch;
            }

            /// <summary>
            /// Get a pushed char, if present, or else return -1.
            /// </summary>
            /// <returns>The pushed character.</returns>
            internal int Pop()
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
            internal int Get()
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
            /// If there is none, return -1.
            /// </summary>
            /// <returns>The next character (as an int).</returns>
            internal int Peek()
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
