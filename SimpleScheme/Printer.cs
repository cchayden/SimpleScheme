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

            switch (x.GetType().FullName)
            {
                // Names for types implementing Scheme values, used for error messages.
                case "System.Boolean":
                    SchemeBoolean.Truth(x).PrintString(quoted, buf);
                    return;
                case "System.Char":
                    x.AsCharacter().PrintString(quoted, buf);
                    return;
                case "System.Object[]":
                    x.AsVector().PrintString(quoted, buf);
                    return;
                case "System.Byte":
                case "System.Int32":
                case "System.Int16":
                case "System.Int64":
                case "System.Single": 
                case "System.Double":
                    x.AsNumber().PrintString(quoted, buf);
                    return;
                case "System.Char[]":
                    x.AsSchemeString().PrintString(quoted, buf);
                    return;
                case "SimpleScheme.Symbol":
                case "SimpleScheme.Pair":
                case "SimpleScheme.Procedure":
                case "SimpleScheme.Primitive":
                case "SimpleScheme.Continuation":
                case "SimpleScheme.Lambda":
                case "SimpleScheme.Macro":
                case "SimpleScheme.InputPort":
                case "SimpleScheme.OutputPort":
                case "SimpleScheme.EmptyList":
                case "SimpleScheme.Evaluator":
                case "SimpleScheme.Undefined":
                    ((Printable)x).PrintString(quoted, buf);
                    return;
                default:
                    // use the built-in ToString
                    buf.Append(x);   
                    return;
            }
        }

        /// <summary>
        /// Gets the primitive type name of an object.
        /// </summary>
        /// <param name="obj">The object to get the type name of.</param>
        /// <returns>The type name.</returns>
        public static string TypeName(Obj obj)
        {
            if (obj.IsPair())
            {
                return Primitive.ValueType.Pair.ToString();
            }

            if (obj.IsEmptyList())
            {
                return Primitive.ValueType.Empty.ToString();
            }

            if (obj.IsNumber())
            {
                return Primitive.ValueType.Number.ToString();
            }

            if (obj.IsCharacter())
            {
                return Primitive.ValueType.Char.ToString();
            }

            if (obj.IsSchemeString())
            {
                return Primitive.ValueType.String.ToString();
            }

            if (obj.IsProcedure())
            {
                return Primitive.ValueType.Proc.ToString();
            }

            if (obj.IsVector())
            {
                return Primitive.ValueType.Vector.ToString();
            }

            if (obj.IsSymbol())
            {
                return Primitive.ValueType.Symbol.ToString();
            }

            if (obj.IsSchemeBoolean())
            {
                return Primitive.ValueType.Boolean.ToString();
            }

            if (obj.IsInputPort() || obj.IsOutputPort())
            {
                return Primitive.ValueType.Port.ToString();
            }

            if (obj.IsAsynchronousClrProcedure())
            {
                return Primitive.ValueType.AsynchronousClrProcedure.ToString();
            }

            if (obj.IsSynchronousClrProcedure())
            {
                return Primitive.ValueType.SynchronousClrProcedure.ToString();
            }

            if (obj.IsClrConstructor())
            {
                return Primitive.ValueType.ClrConstructor.ToString();
            }

            if (obj.IsContinuation())
            {
                return Primitive.ValueType.Continuation.ToString();
            }

            if (obj.IsLambda())
            {
                return Primitive.ValueType.Lambda.ToString();
            }

            if (obj.IsMacro())
            {
                return Primitive.ValueType.Macro.ToString();
            }

            if (obj.IsUndefined())
            {
                return Primitive.ValueType.Undefined.ToString();
            }

            return "Unknown";
        }
        #endregion
    }
}