// <copyright file="Printer.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// In charge of printing values.
    /// Can print in one of two forms: quoted or unquoted.
    /// Quoted values are more verbose, and are appropriate for logs or traces.
    /// Unquoted values are for use internally (for instance to get a filename from either a 
    ///    string or a symbol.)
    /// </summary>
    public class Printer
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
            StringBuilder buf = new StringBuilder();
            AsString(x, quoted, buf);
            return buf.ToString();
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// First gets the actual type, then calls a type-specific routine.
        /// If not one of the predefined types, use generic ToString.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void AsString(Obj x, bool quoted, StringBuilder buf)
        {
            if (x == null)
            {
                return;
            }

            switch (x.GetType().FullName)
            {
                // Names for types implementing Scheme values, used for error messages.
                case "System.Boolean":
                    SchemeBoolean.AsString(SchemeBoolean.Truth(x), quoted, buf);
                    return;
                case "System.String":
                    Symbol.AsString(Symbol.As(x), quoted, buf);
                    return;
                case "System.Char":
                    Character.AsString(Character.As(x), quoted, buf);
                    return;
                case "System.Object[]":
                    Vector.AsString(Vector.As(x), quoted, buf);
                    return;
                case "System.Byte":
                case "System.Int32":
                case "System.Int16":
                case "System.Int64":
                case "System.Single": 
                case "System.Double":
                    Number.AsString(Number.As(x), quoted, buf);
                    return;
                case "System.Char[]":
                    SchemeString.AsString(SchemeString.As(x), quoted, buf);
                    return;
                case "SimpleScheme.Pair":
                case "SimpleScheme.Procedure":
                case "SimpleScheme.Primitive":
                case "SimpleScheme.Continuation":
                case "SimpleScheme.Closure":
                case "SimpleScheme.Macro":
                case "SimpleScheme.InputPort":
                case "SimpleScheme.OutputPort":
                case "SimpleScheme.EmptyList":
                case "SimpleScheme.Stepper":
                case "SimpleScheme.Undefined":
                    ((Printable)x).AsString(quoted, buf);
                    return;
                default:
                    // use the built-in ToString
                    buf.Append(x);   
                    return;
            }
        }
        #endregion
     }
}