// <copyright file="Token.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>

namespace SimpleScheme
{
    using System.Diagnostics.CodeAnalysis;

    /// <summary>
    /// There are the only tokens in the language.
    /// </summary>
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1602:EnumerationItemsMustBeDocumented", Justification = "Token types described below.")]
    internal enum TokType
    {
        LParen,
        RParen,
        Dot,
        Comma,
        SingleQuote,
        BackQuote,
        Splice,
        OpenVector,
        Eof,
        None
    }

    /// <summary>
    /// Tokens are returned from NextToken.
    /// They are not scheme objects, but are handled internally by the parser
    ///   along side scheme objects.  Eventually Read returns scheme objects, not tokens.
    /// Tokens are generally scheme punctuation such as ( ) . , ' and `.
    /// They also include multi-char tokens such as ,@ and Eof.
    /// </summary>
    internal class Token : SchemeObject
    {
        /// <summary>
        /// Left paren.
        /// </summary>
        internal static readonly Token LParen = new Token(TokType.LParen, "(");

        /// <summary>
        /// Right paren.
        /// </summary>
        internal static readonly Token RParen = new Token(TokType.RParen, ")");

        /// <summary>
        /// Dot token.
        /// </summary>
        internal static readonly Token Dot = new Token(TokType.Dot, ".");

        /// <summary>
        /// Comma token.
        /// </summary>
        internal static readonly Token Comma = new Token(TokType.Comma, ",");

        /// <summary>
        /// Single quote.
        /// </summary>
        internal static readonly Token SingleQuote = new Token(TokType.SingleQuote, "'");

        /// <summary>
        /// Back quote.
        /// </summary>
        internal static readonly Token BackQuote = new Token(TokType.BackQuote, "`");

        /// <summary>
        /// Splice token.
        /// </summary>
        internal static readonly Token Splice = new Token(TokType.Splice, ",@");

        /// <summary>
        /// Open a vector constant.
        /// </summary>
        internal static readonly Token OpenVector = new Token(TokType.OpenVector, "#(");

        /// <summary>
        /// End of file.
        /// </summary>
        internal static readonly Token Eof = new Token(TokType.Eof, "#EOF!");

        #region Fields
        /// <summary>
        /// Identifies the particular token.
        /// </summary>
        private readonly TokType tokType;

        /// <summary>
        /// The string representation of the token.
        /// </summary>
        private readonly string tokString;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the <see cref="Token"/> class.
        /// </summary>
        /// <param name="tokType">The value.</param>
        /// <param name="tokString">The token string value.</param>
        private Token(TokType tokType, string tokString)
        {
            this.tokType = tokType;
            this.tokString = tokString;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the token type.
        /// </summary>
        internal TokType TokType
        {
            get { return this.tokType; }
        }
        #endregion

        /// <summary>
        /// The token value: the string version of the enum.
        /// </summary>
        /// <returns>The string value of the token.</returns>
        public override string ToString()
        {
            return this.tokString;
        }
    }
}