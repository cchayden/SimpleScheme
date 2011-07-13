// <copyright file="EmptyList.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// This represents the empty list.
    /// The empty list could just as well be represented by null, but that loses some type
    ///   safety, since it is compatible with any type.
    /// </summary>
    internal class EmptyList
    {
        /// <summary>
        /// Print the empty list.
        /// </summary>
        /// <returns>The empty list token.</returns>
        public override string ToString()
        {
            return "<empty>";
        }

        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is an empty list.</returns>
        internal static bool IsType(Obj obj)
        {
            return obj is EmptyList;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string TypeName()
        {
            return "empty list";
        }
    }
}