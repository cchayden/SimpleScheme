// <copyright file="Undefined.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

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
        /// Tests whether to given object is a scheme undefined object.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme undefined object.</returns>
        public static bool IsUndefined(Obj obj)
        {
            return obj is Undefined;
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Convert object to undefined object.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as an undefined object.</returns>
        internal static Undefined AsUndefined(Obj obj)
        {
            return (Undefined)obj;
        }
        #endregion
    }

    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    internal static partial class Extensions
    {
        /// <summary>
        /// Write the undefined object to the string builder.
        /// If not quoted, write nothing.
        /// One of the main reasons for this type is to suppress output.
        /// </summary>
        /// <param name="undef">The undefined object (not used).</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        internal static void AsString(this Undefined undef, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<undefined>");
            }
        }
    }
}
