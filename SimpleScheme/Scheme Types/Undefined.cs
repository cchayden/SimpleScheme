﻿// <copyright file="Undefined.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    using Obj = System.Object;

    /// <summary>
    /// Represents an undefined scheme value.
    /// This type is immutable.
    /// </summary>
    public class Undefined : Printable
    {
        #region Constants

        /// <summary>
        /// The printable name of the undefined type.
        /// </summary>
        public const string Name = "undefined";

        #endregion

        /// <summary>
        /// Prevents a default instance of the <see cref="Undefined"/> class from being created. 
        /// </summary>
        private Undefined()
        {
        }

        #region Public Static Methods
        /// <summary>
        /// Create a new undefined object.
        /// </summary>
        /// <returns>A new undefined object.</returns>
        public static Undefined New()
        {
            return new Undefined();
        }

        #endregion

        #region Public Methods

        /// <summary>
        /// Write the undefined object to the string builder.
        /// If not quoted, write nothing.
        /// One of the main reasons for this type is to suppress output.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append(this.ToString());
            }
        }

        /// <summary>
        /// Display the value as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The undefined type name.</returns>
        public override string ToString()
        {
            return "<" + Name + ">";
        }

        #endregion
    }

    /// <summary>
    /// Extensions for Undefined
    /// </summary>
    public static class UndefinedExtensions
    {
        /// <summary>
        /// Tests whether to given object is a scheme undefined object.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme undefined object.</returns>
        public static bool IsUndefined(this Obj obj)
        {
            return obj is Undefined;
        }

        /// <summary>
        /// Convert object to undefined object.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as an undefined object.</returns>
        public static Undefined AsUndefined(this Obj obj)
        {
            return (Undefined)obj;
        }
    }    
}
