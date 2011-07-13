// <copyright file="TypePrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Primitive operations on built-in scheme types.
    /// </summary>
    public class TypePrimitives
    {
        /// <summary>
        /// Find the scheme type name for a given object.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>The scheme type name.</returns>
        internal static string TypeName(Obj obj)
        {
            if (SchemeBoolean.IsType(obj))
            {
                return SchemeBoolean.TypeName();
            }

            if (Symbol.IsType(obj))
            {
                return Symbol.TypeName();
            }

            if (Character.IsType(obj))
            {
                return Character.TypeName();
            }

            if (Vector.IsType(obj))
            {
                return Vector.TypeName();
            }

            if (Pair.IsType(obj))
            {
                return Pair.TypeName();
            }

            if (Number.IsType(obj))
            {
                return Number.TypeName();
            }

            if (SchemeString.IsType(obj))
            {
                return SchemeString.TypeName();
            }

            if (Procedure.IsType(obj))
            {
                return Procedure.TypeName();
            }

            if (InputPort.IsType(obj))
            {
                return InputPort.TypeName();
            }

            if (OutputPort.IsType(obj))
            {
                return OutputPort.TypeName();
            }

            if (EmptyList.IsType(obj))
            {
                return EmptyList.TypeName();
            }

            return obj.GetType().ToString();
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        internal static void AsString(Obj x, bool quoted, StringBuilder buf)
        {
            if (EmptyList.IsType(x))
            {
                buf.Append("()");
            }
            else if (Number.IsType(x))
            {
                Number.AsString(x, quoted, buf);
            }
            else if (Character.IsType(x))
            {
                Character.AsString(x, quoted, buf);
            }
            else if (Pair.IsType(x))
            {
                Pair.AsString(x, quoted, buf);
            }
            else if (Symbol.IsType(x))
            {
                Symbol.AsString(x, quoted, buf);
            }
            else if (SchemeString.IsType(x))
            {
                SchemeString.AsString(x, quoted, buf);
            }
            else if (Vector.IsType(x))
            {
                Vector.AsString(x, quoted, buf);
            }
            else if (SchemeBoolean.IsType(x))
            {
                SchemeBoolean.AsString(x, quoted, buf);
            }
            else if (x is Stepper)
            {
                Stepper.AsString(quoted, buf);
            }
            else if (x is Undefined)
            {
                Undefined.AsString(quoted, buf);
            }
            else
            {
                buf.Append(x);
            }
        }
    }
}
