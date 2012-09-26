﻿// <copyright file="Undefined.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Represents an undefined scheme value.
    /// This type is immutable.
    /// </summary>
    public class Undefined : IPrintable, ISchemeObject
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

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Undefined); }
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
            return "<undefined>";
        }

        #endregion
    }

    /// <summary>
    /// Extensions for Undefined
    /// </summary>
    public static class UndefinedExtension
    {
        /// <summary>
        /// Convert object to undefined object.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as an undefined object.</returns>
        public static Undefined AsUndefined(this ISchemeObject obj)
        {
            if (obj is Undefined)
            {
                return (Undefined)obj;
            }

            ErrorHandlers.TypeError(typeof(Undefined), obj);
            return null;
        }
    }    
}
