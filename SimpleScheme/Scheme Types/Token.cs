// <copyright file="Token.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>

namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Text;

    /// <summary>
    /// Tokens are returned from NextToken.
    /// They are not scheme objects, but are handled internally by the parser
    ///   along side scheme objects.  Eventually Read returns scheme objects, not tokens.
    /// Tokens are generally scheme punctuation such as ( ) . , ' and `.
    /// They also include multi-char tokens such as ,@ and Eof.
    /// </summary>
    public class Token : SchemeObject
    {
        /// <summary>
        /// Cache of predefined tokens, used when possible.
        /// </summary>
        private static readonly Dictionary<string, Token> tokens;

        /// <summary>
        /// Identifies the particular token.
        /// </summary>
        private readonly string value;

        /// <summary>
        /// Initializes static members of the <see cref="Token"/> class.
        /// </summary>
        static Token()
        {
            tokens = new Dictionary<string, Token>(8);
            foreach (var s in new[] { "(", ")", ".", ",", "'", "`", ",@", "#EOF!" })
            {
               tokens.Add(s, new Token(s)); 
            }
        }

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
        public override string TypeName
        {
            get
            {
                return "Token";
            }
        }

        /// <summary>
        /// Gets a token.  Either retrieves an existing token instance,
        ///   or creates a new one.
        /// </summary>
        /// <param name="tok">The token (as a string).</param>
        /// <returns>The Token.</returns>
        public static Token New(string tok)
        {
            Token t;
            if (tokens.TryGetValue(tok, out t))
            {
                return t;
            }

            return new Token(tok);
        }

        /// <summary>
        /// Print the token.
        /// </summary>
        /// <param name="quoted">True to print quoted.</param>
        /// <param name="buf">Buffer to print to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
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