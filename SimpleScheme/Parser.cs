﻿// <copyright file="Parser.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
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
        /// The TextReader we are reading from to get the input.
        /// </summary>
        private readonly TextReader inp;

        /// <summary>
        /// The character buffer to read from.
        /// </summary>
        private Tuple<bool, int> charBuffer = new Tuple<bool, int>(false, -1);

        /// <summary>
        /// The token buffer to read from.
        /// </summary>
        private Tuple<bool, object> tokBuffer = new Tuple<bool, object>(false, null);

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
                int ch = Get(this.charBuffer);
                this.charBuffer = new Tuple<bool, int>(false, -1);
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
            object token = this.tokBuffer.Item1 ? this.tokBuffer.Item2 : null;
            this.tokBuffer = new Tuple<bool, object>(false, null);
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

                    this.charBuffer = new Tuple<bool, int>(true, ch);
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
                            this.charBuffer = new Tuple<bool, int>(true, '(');
                            return Vector.FromList(this.Read());

                        case '\\':
                            ch = this.ReadNextChar();
                            if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N')
                            {
                                this.charBuffer = new Tuple<bool, int>(true, ch);
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
                                        this.tokBuffer = new Tuple<bool, object>(true, token);
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

                        this.charBuffer = new Tuple<bool, int>(true, ch);
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

        #region Private Static Methods
        /// <summary>
        /// Get a character from the character buffer.
        /// </summary>
        /// <param name="bc">The character buffer.</param>
        /// <returns>The buffered character.</returns>
        private static int Get(Tuple<bool, int> bc)
        {
                if (bc.Item1)
                {
                    return bc.Item2 == -1 ? -1 : Character.As((char)bc.Item2);
                }

                return -2;
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
            int ch = this.charBuffer.Item1 ? this.charBuffer.Item2 : -1;
            if (ch != -1)
            {
                return ch;
            }

            try
            {
                ch = this.inp.Read();
                this.charBuffer = new Tuple<bool, int>(true, ch);
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
            int ch = this.charBuffer.Item1 ? this.charBuffer.Item2 : -1;
            this.charBuffer = new Tuple<bool, int>(false, -1);
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
            int ch = this.charBuffer.Item1 ? this.charBuffer.Item2 : -1;
            this.charBuffer = new Tuple<bool, int>(false, -1);
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

            this.tokBuffer = new Tuple<bool, object>(true, token);
            return Pair.Cons(this.Read(), this.ReadTail(true));
        }
        #endregion
    }
}
