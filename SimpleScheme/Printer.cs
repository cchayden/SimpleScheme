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
                case "SimpleScheme.Lambda":
                case "SimpleScheme.Macro":
                case "SimpleScheme.InputPort":
                case "SimpleScheme.OutputPort":
                case "SimpleScheme.EmptyList":
                case "SimpleScheme.Evaluator":
                case "SimpleScheme.Undefined":
                    ((Printable)x).AsString(quoted, buf);
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
            if (Pair.Is(obj))
            {
                return Primitive.ValueType.Pair.ToString();
            }

            if (EmptyList.Is(obj))
            {
                return Primitive.ValueType.Empty.ToString();
            }

            if (Number.Is(obj))
            {
                return Primitive.ValueType.Number.ToString();
            }

            if (Character.Is(obj))
            {
                return Primitive.ValueType.Char.ToString();
            }

            if (SchemeString.Is(obj))
            {
                return Primitive.ValueType.String.ToString();
            }

            if (Procedure.Is(obj))
            {
                return Primitive.ValueType.Proc.ToString();
            }

            if (Vector.Is(obj))
            {
                return Primitive.ValueType.Vector.ToString();
            }

            if (Symbol.Is(obj))
            {
                return Primitive.ValueType.Symbol.ToString();
            }

            if (SchemeBoolean.Is(obj))
            {
                return Primitive.ValueType.Boolean.ToString();
            }

            if (InputPort.Is(obj) || OutputPort.Is(obj))
            {
                return Primitive.ValueType.Port.ToString();
            }

            if (AsynchronousClrProcedure.Is(obj))
            {
                return Primitive.ValueType.AsynchronousClrProcedure.ToString();
            }

            if (SynchronousClrProcedure.Is(obj))
            {
                return Primitive.ValueType.SynchronousClrProcedure.ToString();
            }

            if (ClrConstructor.Is(obj))
            {
                return Primitive.ValueType.ClrConstructor.ToString();
            }

            if (Continuation.Is(obj))
            {
                return Primitive.ValueType.Continuation.ToString();
            }

            if (Lambda.Is(obj))
            {
                return Primitive.ValueType.Lambda.ToString();
            }

            if (Macro.Is(obj))
            {
                return Primitive.ValueType.Macro.ToString();
            }

            if (Undefined.Is(obj))
            {
                return Primitive.ValueType.Undefined.ToString();
            }

            return "Unknown";
        }
        #endregion
    }
}