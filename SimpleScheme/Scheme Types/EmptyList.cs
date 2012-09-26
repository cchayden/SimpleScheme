// <copyright file="EmptyList.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// This represents the empty list.
    /// It is immutable.
    /// The empty list could just as well be represented by null, but that loses some type
    ///   safety, since it is compatible with any type.
    /// </summary>
    public class EmptyList : SchemeObject
    {
        /// <summary>
        /// The empty list is represented by a distinguished obj.
        /// It would also work to have the empty list be represeted by null.
        /// </summary>
        private static readonly EmptyList instance = new EmptyList();

        #region Constructor
        /// <summary>
        /// Prevents a default instance of the EmptyList class from being created.
        /// </summary>
        private EmptyList()
        {
        }
        #endregion

        #region Instance
        /// <summary>
        /// Gets the single instance.
        /// </summary>
        /// <returns>An empty list.</returns>
        public static EmptyList Instance
        {
            get { return instance; }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the empty list to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Print the empty list.
        /// </summary>
        /// <returns>The empty list token.</returns>
        public override string ToString()
        {
            return "()";
        }
        #endregion
    }
}