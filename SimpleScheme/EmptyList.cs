// <copyright file="EmptyList.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// This represents the empty list.
    /// The empty list could just as well be represented by null, but that loses some type
    ///   safety, since it is compatible with any type.
    /// </summary>
    internal class EmptyList
    {
        #region Constants
        /// <summary>
        /// The empty list is represented by a distinguished obj.
        /// It would also work to have the empty list be represeted by null.
        /// </summary>
        public static readonly Obj Instance = new EmptyList();
        #endregion

        #region Public Methods
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