// <copyright file="Undefined.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Represents an undefined scheme value.
    /// </summary>
    public class Undefined
    {
        #region Fields
        /// <summary>
        /// Keep one instance of this around to use when needed.
        /// </summary>
        public static readonly Undefined Instance = new Undefined();
        #endregion

        #region Constructor
        /// <summary>
        /// Prevents a default instance of the Undefined class from being created.
        /// </summary>
        private Undefined()
        {
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Convert the undefined instance a string.
        /// </summary>
        /// <param name="quoted">True if the string should be quoted.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void AsString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<undefined>");
            }
        }
        #endregion
    }
}
