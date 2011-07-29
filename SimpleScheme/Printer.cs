// <copyright file="Printer.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
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
                    SchemeBoolean.Truth(x).AsString(quoted, buf);
                    return;
                case "System.String":
                    Symbol.AsSymbol(x).AsString(quoted, buf);
                    return;
                case "System.Char":
                    Character.AsCharacter(x).AsString(quoted, buf);
                    return;
                case "System.Object[]":
                    Vector.AsVector(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Pair":
                    Pair.AsPair(x).AsString(quoted, buf);
                    return;
                case "System.Byte":
                case "System.Int32":
                case "System.Int16":
                case "System.Int64":
                case "System.Single": 
                case "System.Double":
                    Number.Num(x).AsString(quoted, buf);
                    return;
                case "System.Char[]":
                    SchemeString.Str(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Procedure":
                    Procedure.AsProcedure(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Primitive":
                    Primitive.AsPrimitive(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Continuation":
                    Continuation.AsContinuation(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Closure":
                    Closure.AsClosure(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Macro":
                    Macro.AsMacro(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.InputPort":
                    InputPort.AsInputPort(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.OutputPort":
                    OutputPort.AsOutputPort(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.EmptyList":
                    EmptyList.AsEmptyList(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Stepper":
                    Stepper.AsStepper(x).AsString(quoted, buf);
                    return;
                case "SimpleScheme.Undefined":
                    Undefined.AsUndefined(x).AsString(quoted, buf);
                    return;
                default:
                    // use the built-in ToString if not in the table
                    buf.Append(x);   
                    return;
            }
        }
        #endregion
     }
}