// <copyright file="Undefined.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Represents an undefined scheme value.
    /// This type is immutable.
    /// </summary>
    public class Undefined : SchemeObject
    {
        /// <summary>
        /// The undefined object instance.
        /// </summary>
        private static readonly Undefined undefined = new Undefined();

        #region Constructors
        /// <summary>
        /// Prevents a default instance of the <see cref="Undefined"/> class from being created. 
        /// </summary>
        private Undefined()
        {
        }

        #endregion

        #region Instance
        /// <summary>
        /// Gets an undefined object.
        /// </summary>
        /// <returns>An undefined object.</returns>
        public static Undefined Instance
        {
            get { return undefined; }
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Display the value as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The undefined type name.</returns>
        public override string ToString()
        {
            return "<undefined>";
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Write the undefined object to the string builder.
        /// If not quoted, write nothing.
        /// One of the main reasons for this type is to suppress output.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <returns>If qupted, then <undefined> otherwise nothing.</undefined></returns>
        internal override string ToString(bool quoted)
        {
            return quoted ? this.ToString() : string.Empty;
        }
        #endregion
    }
}
