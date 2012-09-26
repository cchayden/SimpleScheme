// <copyright file="Token.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>

namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Tokens are returned from NextToken.
    /// They are not scheme objects, but are handled internally by the parser
    ///   along side scheme objects.  Eventually Read returns scheme objects, not tokens.
    /// Tokens are generally scheme punctuation such as ( ) . , ' and `.
    /// They also include multi-char tokens such as ,@ and Eof.
    /// </summary>
    internal class Token : SchemeObject
    {
        #region Fields
        /// <summary>
        /// Cache of predefined tokens, used when possible.
        /// </summary>
        private static readonly Dictionary<string, Token> tokens;

        /// <summary>
        /// Identifies the particular token.
        /// </summary>
        private readonly string value;
        #endregion

        #region Constructors
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
        internal Token(string value)
        {
            Contract.Requires(value != null);
            this.value = value;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// The token value.
        /// </summary>
        /// <returns>The string value of the token.</returns>
        public override string ToString()
        {
            return this.value;
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Gets a token.  Either retrieves an existing token instance,
        ///   or creates a new one.
        /// </summary>
        /// <param name="tok">The token (as a string).</param>
        /// <returns>The Token.</returns>
        internal static Token New(string tok)
        {
            Contract.Requires(tok != null);
            Contract.Ensures(Contract.Result<Token>() != null);
            Token t;
            if (tokens.TryGetValue(tok, out t))
            {
                Contract.Assume(t != null);
                return t;
            }

            return new Token(tok);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.value != null);
        }
        #endregion
    }
}