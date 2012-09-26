// <copyright file="Parser.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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

        #region Fields

        /// <summary>
        /// Lexical analyzer.  Scans input for tokens.
        /// </summary>
        private readonly Scanner scanner;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Parser class.
        /// </summary>
        /// <param name="inp">The input TextReader we are reading from.</param>
        internal Parser(TextReader inp)
        {
            this.scanner = new Scanner(inp, Prompt);
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the internal TextReader object.
        /// </summary>
        internal TextReader Reader
        {
            get { return this.scanner.Inp; }
        }

        /// <summary>
        /// Gets the current line number the scanner is processing.
        /// </summary>
        internal int LineNumber
        {
            get { return this.scanner.LineNumber; }
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Close the input.
        /// </summary>
        internal void Close()
        {
            this.scanner.Close();
        }

        /// <summary>
        /// Peek at the next character to be read.
        /// </summary>
        /// <returns>The nest character (as a Character).</returns>
        internal SchemeObject PeekChar()
        {
            return this.scanner.PeekChar();
        }

        /// <summary>
        /// Read and consume the next character from the input.
        /// </summary>
        /// <returns>The character that was read.</returns>
        internal SchemeObject ReadChar()
        {
            return this.scanner.ReadChar();
        }

        /// <summary>
        /// Get the next token.
        /// Used only in tests.
        /// </summary>
        /// <returns>The next token from the scanner.</returns>
        internal SchemeObject NextToken()
        {
            return this.scanner.NextToken();
        }

        /// <summary>
        /// Push a token back on the input.
        /// </summary>
        /// <param name="token">The token to push back.</param>
        internal void PushToken(SchemeObject token)
        {
            this.scanner.PushToken(token);
        }

        /// <summary>
        /// Read a complete expression.
        /// </summary>
        /// <param name="sb">The characters read are recorded in this StringBuilder.</param>
        /// <returns>The expression that was read.</returns>
        internal SchemeObject ReadExpr(StringBuilder sb)
        {
            this.scanner.Logger = sb;
            return this.Read();
        }

        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        internal SchemeObject Read()
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
                        case "#(":
                            token = Vector.FromList(this.Read(), this.LineNumber);
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
        #endregion

        #region Private Methods
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

            this.PushToken(token);
            return List.Cons(this.Read(), this.ReadTail(true), this.LineNumber);
        }
        #endregion
    }
}
