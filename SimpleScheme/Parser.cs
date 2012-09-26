// <copyright file="Parser.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Text;

    /// <summary>
    /// Parse scheme expressions.
    /// </summary>
    internal class Parser
    {
        #region Constants
        /// <summary>
        /// The interactive prompt.
        /// </summary>
        private const string Prompt = "{0} > ";
        #endregion

        #region Static Fields
        /// <summary>
        /// Store the names and values of all named characters.
        /// </summary>
        private static readonly Dictionary<string, char> CharNames = new Dictionary<string, char>
            {
                { "nul", '\x0000' },
                { "soh", '\x0001' },
                { "stx", '\x0002' },
                { "etx", '\x0003' },
                { "eot", '\x0004' },
                { "enq", '\x0005' },
                { "ack", '\x0006' },
                { "bel", '\x0007' },
                { "bs", '\x0008' },
                { "ht", '\x0009' },
                { "nl", '\x000a' },
                { "vt", '\x000b' },
                { "np", '\x000c' },
                { "cr", '\x000d' },
                { "so", '\x000e' },
                { "si", '\x000f' },
                { "dle", '\x0010' },
                { "dc1", '\x0011' },
                { "dc2", '\x0012' },
                { "dc3", '\x0013' },
                { "dc4", '\x0014' },
                { "nack", '\x0015' },
                { "syn", '\x0016' },
                { "etc", '\x0017' },
                { "can", '\x0018' },
                { "em", '\x0019' },
                { "sub", '\x001a' },
                { "esc", '\x001b' },
                { "fs", '\x001c' },
                { "gs", '\x001d' },
                { "rs", '\x001e' },
                { "us", '\x001f' },
                { "del", '\x007f' },
                { "sp", ' ' },
                { "space", ' ' },
                { "tab", '\t' },
                { "newline", '\n' },
                { "return", '\r' }
            };

        #endregion

        #region Fields
        /// <summary>
        /// The TextReader we are reading from to get the input.
        /// </summary>
        private readonly TextReader inp;

        /// <summary>
        /// The character buffer to read from.
        /// </summary>
        private readonly CharacterBuffer charBuffer = new CharacterBuffer();

        /// <summary>
        /// The token buffer to read from.
        /// </summary>
        private readonly TokenBuffer tokBuffer = new TokenBuffer();

        /// <summary>
        /// Buffers characters read from the input stream.
        /// </summary>
        private readonly LineBuffer lineBuffer;

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
        internal Parser(TextReader inp)
        {
            this.inp = inp;
            this.lineBuffer = new LineBuffer(inp);
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the current line number.
        /// </summary>
        internal int LineNumber
        {
            get { return this.lineBuffer.LineNumber; }
        }

        /// <summary>
        /// Gets the internal TextReader object.
        /// </summary>
        internal TextReader Reader
        {
            get
            {
                return this.inp;
            }
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Read a complete expression.
        /// </summary>
        /// <param name="sb">The characters read are recorded in this StringBuilder.</param>
        /// <returns>The expression that was read.</returns>
        internal SchemeObject ReadExpr(StringBuilder sb)
        {
            this.logger = sb;
            return this.Read();
        }

        /// <summary>
        /// Take a peek at the next character, without consuming it.
        /// </summary>
        /// <returns>The next character (as a scheme character).</returns>
        internal SchemeObject PeekChar()
        {
            int p = this.Peek();
            if (p == -1)
            {
                return Eof.Instance;
            }

            return (Character)(char)p;
        }

        /// <summary>
        /// Read a character from the input port.
        /// Gets a pushed character, if present.
        /// </summary>
        /// <param name="sb">The characters read are recorded in this StringBuilder.</param>
        /// <returns>The character read, or EOF.</returns>
        internal SchemeObject ReadChar(StringBuilder sb)
        {
            this.logger = sb;
            try
            {
                int ch = this.charBuffer.Get();
                if (ch == -2)
                {
                    ch = this.ReadNextChar();
                }

                if (ch == -1)
                {
                    return Eof.Instance;
                }

                return (Character)(char)ch;
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception: " + ex);
                return Eof.Instance;
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
        /// Get the next word -- the next string of alphabetic characters.
        /// </summary>
        /// <param name="test">Predicate for letters that make up a word.  Use with IsHexDigit, char.IsLetter, etc.</param>
        /// <returns>The next word.  Could be empty string.</returns>
        internal string NextWord(Func<char, bool> test)
        {
            var sb = new StringBuilder();
            while (true)
            {
                int ch = this.ReadOrPop();
                if (!test((char)ch))
                {
                    this.charBuffer.Push(ch);
                    return sb.ToString();
                }

                sb.Append((char)ch);
            }
        }

        /// <summary>
        /// Gets the next token from the input stream.
        /// Gets a pushed token if there is one, otherwise reads from the input.
        /// </summary>
        /// <returns>The next token.</returns>
        internal SchemeObject NextToken()
        {
            // See if we should re-use a pushed token or character
            SchemeObject token = this.tokBuffer.Get();
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
                    // end of file
                    return Token.New("#EOF!");

                case '(':
                    return Token.New("(");
                case ')':
                    return Token.New(")");
                case '`':
                    return Token.New("`");
                case '\'':
                    return Token.New("'");

                case ',':
                    // read comma or unquote splicing
                    ch = this.ReadNextChar();
                    if (ch == '@')
                    {
                        return Token.New(",@");
                    }

                    this.charBuffer.Push(ch);
                    return Token.New(",");

                case ';':
                    // skip comment
                    this.NextWord(IsCommentChar);
                    return this.NextToken();

                case '"':
                    // read a string
                    {
                        var buff = new StringBuilder { Length = 0 };
                        while ((ch = this.ReadNextChar()) != '"' & ch != -1)
                        {
                            buff.Append((char)((ch == '\\') ? this.ReadNextChar() : ch));
                        }

                        if (ch == -1)
                        {
                            ErrorHandlers.Warn("EOF inside of a string.");
                        }

                        return SchemeString.New(buff, this.LineNumber);
                    }

                case '#':
                    // read a boolean, vector, number, or character
                    switch (ch = this.ReadNextChar())
                    {
                        case 't':
                        case 'T':
                            return SchemeBoolean.New(true, this.LineNumber);

                        case 'f':
                        case 'F':
                            return SchemeBoolean.New(false, this.LineNumber);

                        case '(':
                            this.charBuffer.Push(ch);
                            return Vector.FromList(this.Read(), this.LineNumber);

                        case '\\':
                            ch = this.ReadNextChar();
                            if (char.IsLetter((char)ch))
                            {
                                this.charBuffer.Push(ch);
                                string tok = this.NextWord(char.IsLetter);
                                if (tok.Length == 1)
                                {
                                    return Character.New((char)ch, this.LineNumber);
                                }

                                tok = tok.ToLower();
                                if (CharNames.ContainsKey(tok))
                                {
                                    return Character.New(CharNames[tok], this.LineNumber);
                                }

                                // At this point there is a token and also a buffered character
                                this.tokBuffer.Push(new Token(tok.Substring(1)));
                                return Character.New((char)ch, this.LineNumber); 
                            }

                            return Character.New((char)ch, this.LineNumber);

                        case 'e':
                        case 'i':
                        case 'd':
                            return Number.New(Convert.ToInt32(this.NextWord(IsDecimalDigit), 10), this.LineNumber);

                        case 'b':
                            return Number.New(Convert.ToInt32(this.NextWord(IsBinaryDigit), 2), this.LineNumber);

                        case 'o':
                            return Number.New(Convert.ToInt32(this.NextWord(IsOctalDigit), 8), this.LineNumber);

                        case 'x':
                            return Number.New(Convert.ToInt32(this.NextWord(IsHexDigit), 16), this.LineNumber);

                        default:
                            ErrorHandlers.Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken();
                    }

                default:
                    {
                        // read a number or symbol
                        int c = ch;
                        this.charBuffer.Push(ch);
                        string buf = this.NextWord(IsSymbolChar);

                        // read a number
                        if (char.IsDigit((char)c) || c == '.' || c == '+' || c == '-')
                        {
                            double value;
                            if (double.TryParse(buf, out value))
                            {
                                return Number.New(value, this.LineNumber);
                            }
                        }

                        if (buf == ".")
                        {
                            return Token.New(".");
                        }

                        // read a symbol
                        return Symbol.New(buf.ToLower(), this.LineNumber);
                    }
            }
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Test for hex digit.
        /// </summary>
        /// <param name="ch">The character</param>
        /// <returns>True if the character is an hex digit.</returns>
        private static bool IsHexDigit(char ch)
        {
            return char.IsDigit(ch) || (char.ToLower(ch) >= 'a' && char.ToLower(ch) <= 'f');
        }

        /// <summary>
        /// Test for octal digit.
        /// </summary>
        /// <param name="ch">The character</param>
        /// <returns>True if the character is an octal digit.</returns>
        private static bool IsOctalDigit(char ch)
        {
            return ch >= '0' && ch <= '7';
        }

        /// <summary>
        /// Test for binary digit.
        /// </summary>
        /// <param name="ch">The character</param>
        /// <returns>True if the character is a binary digit.</returns>
        private static bool IsBinaryDigit(char ch)
        {
            return ch == '0' || ch == '1';
        }

        /// <summary>
        /// Test for decimal digit.
        /// </summary>
        /// <param name="ch">The character</param>
        /// <returns>True if the character is a binary digit.</returns>
        private static bool IsDecimalDigit(char ch)
        {
            return ch >= '0' && ch <= '9';
        }

        /// <summary>
        /// Test for symbol characters.  These characters make up symbols -- they are all the characters
        ///   that are not otherwise special.
        /// </summary>
        /// <param name="ch">The character</param>
        /// <returns>True if the character is a symbol character.</returns>
        private static bool IsSymbolChar(char ch)
        {
            return !char.IsWhiteSpace(ch) && (short)ch != -1 && ch != '(' && ch != ')' && ch != '\'' && ch != ';'
                   && ch != '"' && ch != ',' && ch != '`';
        }

        /// <summary>
        /// Test for comment characters.  
        /// </summary>
        /// <param name="ch">The character</param>
        /// <returns>True if the character is a comment character.</returns>
        private static bool IsCommentChar(char ch)
        {
            return (short)ch != -1 && ch != '\n' && ch != '\r';
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        private SchemeObject Read()
        {
            try
            {
                SchemeObject token = this.NextToken();

                if (token is Token)
                {
                    switch (token.ToString())
                    {
                        case "#EOF!":
                            token = Eof.Instance;
                            break;
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
                            token = List.MakeList(Symbol.New("quote", this.LineNumber), this.Read());
                            break;
                        case "`":
                            token = List.MakeList(Symbol.New("quasiquote", this.LineNumber), this.Read());
                            break;
                        case ",":
                            token = List.MakeList(Symbol.New("unquote", this.LineNumber), this.Read());
                            break;
                        case ",@":
                            token = List.MakeList(Symbol.New("unquote-splicing", this.LineNumber), this.Read());
                            break;
                    }
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
            int ch = this.charBuffer.Peek();
            if (ch != -1)
            {
                return ch;
            }

            try
            {
                ch = this.lineBuffer.ReadCharacter();
                this.charBuffer.Push(ch);
                return ch;
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception: " + ex);
                return -1;
            }
        }

        /// <summary>
        /// Pop a buffered character, if one is available.
        /// Otherwise read one from the stream.
        /// </summary>
        /// <returns>The character read.</returns>
        private int ReadOrPop()
        {
            int ch = this.charBuffer.Get();
            if (ch != -2)
            {
                return ch;
            }

            ch = this.lineBuffer.ReadCharacter();

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
            int ch = this.charBuffer.Get();
            if (ch != -2)
            {
                ErrorHandlers.IoError("Read bypassed pushed char.");
            }

            ch = this.lineBuffer.ReadCharacter();
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
        private SchemeObject ReadTail(bool dotOk)
        {
            SchemeObject token = this.NextToken();
            if (token is Token)
            {
                string tok = token.ToString();
                if (tok == "#EOF!")
                {
                    return ErrorHandlers.IoError("EOF during read.");
                }

                if (tok == ")")
                {
                    return EmptyList.Instance; // there was no more
                }

                if (tok == ".")
                {
                    if (!dotOk)
                    {
                        ErrorHandlers.Warn("Dot not allowed here, ignored.");
                        return this.ReadTail(false);
                    }

                    SchemeObject result = this.Read();
                    token = this.NextToken();
                    if (!(token is Token) || token.ToString() != ")")
                    {
                        ErrorHandlers.Warn("Expecting ')' got " + token + " after dot");
                    }

                    return result;
                }
            }

            this.tokBuffer.Push(token);
            return List.Cons(this.Read(), this.ReadTail(true), this.LineNumber);
        }
        #endregion

        #region CharacterBuffer
        /// <summary>
        /// Buffers a single character.
        /// </summary>
        private class CharacterBuffer
        {
            /// <summary>
            /// True if a buffered character is present.
            /// </summary>
            private bool present;

            /// <summary>
            /// If present, the buffered character.
            /// </summary>
            private int ch;

            /// <summary>
            /// Get a character from the character buffer.
            /// </summary>
            /// <returns>The buffered character.  If no character is buffered, return -2.</returns>
            internal int Get()
            {
                int chr = this.present ? this.ch : -2;
                this.Clear();
                return chr;
            }

            /// <summary>
            /// Push the character into the buffer.
            /// </summary>
            /// <param name="chr">The character to push.</param>
            internal void Push(int chr)
            {
                this.present = true;
                this.ch = chr;
            }

            /// <summary>
            /// If there is a char in the buffer, return it (without clearing).
            /// Otherwise, return -1;
            /// </summary>
            /// <returns>The char in the buffer, else -1.</returns>
            internal int Peek()
            {
                return this.present ? this.ch : -1;
            }

            /// <summary>
            /// Clear the character buffer.
            /// </summary>
            private void Clear()
            {
                this.present = false;
                this.ch = -1;
            }
        }
        #endregion

        #region TokenBuffer
        /// <summary>
        /// Reads Tokens.  May read one ahead, so one token may be pushed back.
        /// </summary>
        private class TokenBuffer
        {
            /// <summary>
            /// True if a buffered token is present.
            /// </summary>
            private bool present;

            /// <summary>
            /// If present, a buffered token.
            /// </summary>
            private SchemeObject buffer;

            /// <summary>
            /// Get a token from the token buffer.
            /// </summary>
            /// <returns>The buffered token.  If no token is buffered, return null.</returns>
            internal SchemeObject Get()
            {
                SchemeObject token = this.present ? this.buffer : null;
                this.Clear();
                return token;
            }

            /// <summary>
            /// Push the character into the buffer.
            /// </summary>
            /// <param name="token">The token to push.</param>
            internal void Push(SchemeObject token)
            {
                this.present = true;
                this.buffer = token;
            }

            /// <summary>
            /// Clear the token buffer.
            /// </summary>
            private void Clear()
            {
                this.present = false;
                this.buffer = null;
            }
        }
        #endregion

        #region LineBuffer
        /// <summary>
        /// Reads characters from an input reader.
        /// If Console, prompts and buffers.
        /// The challenge here is to get the line numbers right for interactive input.
        /// ReadExpr() stops when an expression is complete, so it does not consume a following newline, 
        ///   and when the next ReadExpr happens, the input still has the newline in it.  So LineNumber before
        ///   ReadExpr will be one off.
        /// Address this by buffering in the case of Console input, and prompting at a low level.
        /// For non-Console input, do not tinker with things.
        /// In either case, the LineBuffer also keeps track of the input line number.
        /// </summary>
        private class LineBuffer
        {
            /// <summary>
            /// The text reader that is the source of input.
            /// </summary>
            private readonly TextReader inp;

            /// <summary>
            /// Buffer for characters read but not delivered to the caller.
            /// </summary>
            private string buffer;

            /// <summary>
            /// The position in the buffer that we are reading from.
            /// </summary>
            private int pos;

            /// <summary>
            /// Initializes a new instance of the <see cref="LineBuffer"/> class.
            /// </summary>
            /// <param name="inp">
            /// The inp.
            /// </param>
            internal LineBuffer(TextReader inp)
            {
                this.inp = inp;
                this.buffer = string.Empty;
                this.pos = 0;
                this.LineNumber = 1;
            }

            /// <summary>
            /// Gets the current line number.
            /// </summary>
            internal int LineNumber
            {
                get; private set;
            }

            /// <summary>
            /// Read a single character from the input port.
            /// If this is coming from the Console, provide a prompt with the line number.
            /// </summary>
            /// <returns>The next character.  Returns -1 if end of file.</returns>
            internal int ReadCharacter()
            {
                int ch;
                if (this.inp == Console.In)
                {
                    // special treatment for console input
                    while (this.pos >= this.buffer.Length)
                    {
                        Console.Write(Prompt, this.LineNumber);

                        this.buffer = this.inp.ReadLine();
                        this.pos = 0;
                        if (this.buffer == null)
                        {
                            return -1;
                        }

                        // ReadLine strips this, so put it back.
                        this.buffer += System.Environment.NewLine;
                    }

                    ch = this.buffer[pos++];
                }
                else
                {
                    // normal non-console input
                    ch = this.inp.Read();
                }

                if (ch == '\n')
                {
                    this.LineNumber++;
                }

                return ch;
            }
        }
        #endregion
    }
}
