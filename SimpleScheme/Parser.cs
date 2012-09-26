// <copyright file="Parser.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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

        #region Fields
        /// <summary>
        /// Translates quote tokens to quote special forms.
        /// </summary>
        private static readonly Dictionary<TokType, string> QuoteTranslation = new Dictionary<TokType, string>();

        /// <summary>
        /// Lexical analyzer.  Scans input for tokens.
        /// </summary>
        private readonly Scanner scanner;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes static members of the <see cref="Parser"/> class.
        /// </summary>
        static Parser()
        {
            QuoteTranslation.Add(TokType.SingleQuote, "quote");
            QuoteTranslation.Add(TokType.BackQuote, "quasiquote");
            QuoteTranslation.Add(TokType.Comma, "unquote");
            QuoteTranslation.Add(TokType.Splice, "unquote-splicing");
        }

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
            return this.ReadExpr();
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Expand a quote token by reading the expr and sticking it in a list after the appropriate symbol.
        /// </summary>
        /// <param name="tok">The quote token to expand.</param>
        /// <param name="expr">The list of enclosed expressions.</param>
        /// <returns>The expanded quote expression.</returns>
        private SchemeObject Expand(TokType tok, SchemeObject expr)
        {
            var sym = QuoteTranslation[tok];
            return List.MakeList(Symbol.New(sym, this.LineNumber), expr);
        }

        /// <summary>
        /// Read a vector.
        /// </summary>
        /// <param name="expr">The list of enclosed expressions.</param>
        /// <returns>The vector.</returns>
        private SchemeObject MakeVector(SchemeObject expr)
        {
            return Vector.FromList(expr, this.LineNumber);
        }

        /// <summary>
        /// Read a single expression.  This can be a scheme value, a list, or a vector.
        /// ReadExpr and ReadList are mutually recursive.
        /// </summary>
        /// <returns>The object read.</returns>
        private SchemeObject ReadExpr()
        {
            while (true)
            {
                SchemeObject token = this.NextToken();
                TokType tok = token is Token ? ((Token)token).TokType : TokType.None;
                switch (tok)
                {
                    case TokType.None:
                        return token; // not a Token -- just return it
                    case TokType.Eof:
                        return InputPort.Eof;
                    case TokType.LParen:
                        var exprList = this.ReadList();
                        this.ReadClose();
                        return exprList;
                    case TokType.OpenVector:
                        var vectorExprList = this.ReadList();
                        this.ReadClose();
                        return this.MakeVector(vectorExprList);
                    case TokType.RParen:
                    case TokType.Dot:
                        ErrorHandlers.Warn("Extra '" + token + "' ignored.");
                        continue;
                    case TokType.SingleQuote:
                    case TokType.BackQuote:
                    case TokType.Comma:
                    case TokType.Splice:
                        var quoteExpr = this.ReadExpr();
                        return this.Expand(tok, quoteExpr);
                    default:
                        ErrorHandlers.Warn("Unexpected token " + token + ".");
                        return EmptyList.Instance; // can never happen
                }
            }
        }

        /// <summary>
        /// Read a list of expressions.  
        /// We have seen an opening paren already, so  read expressions until a closing paren is seen.
        /// Return the list, leaving the closing paren unread.
        /// The list cannot start with dot, but after the first expression dot is legal.
        /// The list is created in reverse order, then fixed up when it is returned.
        /// </summary>
        /// <returns>The list of expressions.</returns>
        private SchemeObject ReadList()
        {
            bool dotOk = false;
            SchemeObject result = EmptyList.Instance;
            while (true)
            {
                SchemeObject token = this.NextToken();
                TokType tok = token is Token ? ((Token)token).TokType : TokType.None;
                switch (tok)
                {
                    case TokType.Eof:
                        ErrorHandlers.IoError("EOF during read.");
                        return null;  // does not return
                    case TokType.RParen:
                        this.PushToken(token);
                        return Pair.ReverseListInPlace(result);  // List is complete -- put the paren back and return the list.
                    case TokType.Dot:
                        if (dotOk)
                        {
                            // read one more expression only, stick it on the end
                            return Pair.ReverseListInPlace(result, this.ReadExpr());
                        }

                        ErrorHandlers.Warn("Dot not allowed here, ignored.");
                        continue;
                }

                // A Token not one of the above, or a non-Token.
                // Read as a single expression and add to the list.
                dotOk = true;
                this.PushToken(token); 
                result = List.Cons(this.ReadExpr(), result, this.LineNumber);
            }
        }

        /// <summary>
        /// Read a closing paren.  If we get anything else, warn and skip it.
        /// </summary>
        private void ReadClose()
        {
            while (true)
            {
                SchemeObject token = this.NextToken();
                TokType tok = token is Token ? ((Token)token).TokType : TokType.None;
                switch (tok)
                {
                    case TokType.Eof:
                        ErrorHandlers.IoError("EOF during read.");
                        return;  // does not return
                    case TokType.RParen:
                        return;
                    default:
                        ErrorHandlers.Warn("Expecting ')' got '" + token + "'.");
                        continue;
                }
            }
        }
        #endregion
    }
}
