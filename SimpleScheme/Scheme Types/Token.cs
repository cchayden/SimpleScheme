// <copyright file="Token.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Tokens are returned from NextToken.
    /// They are not scheme objects, but are handled internally by the parser
    ///   along side scheme objects.  Eventually Read returns scheme objects, not tokens.
    /// Tokens are generally scheme punctuation such as ( ) . , ' and `.
    /// They also include multi-char tokens such as ,@ and Eof.
    /// </summary>
    public class Token : ISchemeObject
    {
        internal static readonly Token Lparen = new Token("(");

        internal static readonly Token Rparen = new Token(")");

        internal static readonly Token Dot = new Token(".");

        internal static readonly Token Comma = new Token(",");

        internal static readonly Token SingleQuote = new Token("'");

        internal static readonly Token BackQuote = new Token("`");

        internal static readonly Token Splice = new Token(",@");

        /// <summary>
        /// Represents the end of file token.
        /// </summary>
        public static readonly Token Eof = new Token("#EOF!");

        /// <summary>
        /// Identifies the particular token.
        /// </summary>
        private readonly string value;

        /// <summary>
        /// Initializes a new instance of the <see cref="Token"/> class.
        /// </summary>
        /// <param name="value">The value.</param>
        public Token(string value)
        {
            this.value = value;
        }

        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return "Token"; }
        }

        /// <summary>
        /// The token value.
        /// </summary>
        /// <returns>The string value of the token.</returns>
        public override string ToString()
        {
            return this.value;
        }
    }
}