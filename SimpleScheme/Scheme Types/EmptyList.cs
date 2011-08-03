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
    public class EmptyList
    {
        #region Constants
        /// <summary>
        /// The empty list is represented by a distinguished obj.
        /// It would also work to have the empty list be represeted by null.
        /// </summary>
        public static readonly Obj Instance = new EmptyList();

        /// <summary>
        /// The printable name of the empty list type.
        /// </summary>
        public const string Name = "empty list";
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme empty list.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme empty list.</returns>
        public static bool IsEmptyList(Obj obj)
        {
            return obj is EmptyList;
        }

        /// <summary>
        /// Convert object to EmptyList.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as an empty list.</returns>
        public static EmptyList AsEmptyList(Obj obj)
        {
            return (EmptyList)obj;
        }
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

    #region Extensions
    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    public static partial class Extensions
    {
        /// <summary>
        /// Write the empty list to the string builder.
        /// </summary>
        /// <param name="lst">The empty list (not used).</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this EmptyList lst, bool quoted, StringBuilder buf)
        {
            buf.Append("()");
        }
    }
    #endregion
}