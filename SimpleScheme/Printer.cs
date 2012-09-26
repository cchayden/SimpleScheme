// <copyright file="Printer.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using System.Collections.Generic;
    using Obj = System.Object;

    /// <summary>
    /// In charge of printing values.
    /// Can print in one of two forms: quoted or unquoted.
    /// Quoted values are more verbose, and are appropriate for logs or traces.
    /// Unquoted values are for use internally (for instance to get a filename from either a 
    ///    string or a symbol.)
    /// </summary>
    public static class Printer
    {
        #region Public Static Methods
        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <returns>The string representing the obj.</returns>
        public static string AsString(Obj x)
        {
            return AsString(x, true);
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        public static string AsString(Obj x, bool quoted)
        {
            var buf = new StringBuilder();
            PrintString(x, quoted, buf);
            return buf.ToString();
        }

        /// <summary>
        /// Convert an obj into a string representation and write to string builder.
        /// First gets the actual type, then calls a type-specific routine.
        /// If not one of the predefined types, use generic ToString.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void PrintString(Obj x, bool quoted, StringBuilder buf)
        {
            if (x == null)
            {
                return;
            }

            if (x is IPrintable)
            {
                ((IPrintable)x).PrintString(quoted, buf);
            }
            else
            {
                // use the built-in ToString
                buf.Append(x);   
            }
        }

        /// <summary>
        /// Gets the primitive type name of an object.
        /// </summary>
        /// <param name="obj">The object to get the type name of.</param>
        /// <returns>The type name.</returns>
        public static string TypeName(Obj obj)
        {
            if (obj is ISchemeType)
            {
                return ((ISchemeType)obj).TypeName;
            }

            return "Unknown";
        }
        #endregion
    }
}