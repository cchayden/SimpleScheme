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
    /// Can print in one of two forms: qupted or unqupted.
    /// </summary>
    public class Printer
    {
        #region Print Table
        /// <summary>
        /// Table of entries used to map the type to a print function.
        /// These include the scheme types as well as additional implementation types.
        /// </summary>
        private static readonly AsStringEntry[] asStringEntries = 
            {
                new AsStringEntry(obj => SchemeBoolean.IsBoolean(obj),      (x, quoted, buf) => SchemeBoolean.Truth(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Symbol.IsSymbol(obj),       (x, quoted, buf) => Symbol.AsSymbol(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Character.IsCharacter(obj),    (x, quoted, buf) => Character.AsCharacter(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Vector.IsVector(obj),       (x, quoted, buf) => Vector.AsVector(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Pair.IsPair(obj),         (x, quoted, buf) => Pair.AsPair(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Number.IsNumber(obj),       (x, quoted, buf) => Number.Num(x).AsString(quoted, buf)),
                new AsStringEntry(obj => SchemeString.IsString(obj),       (x, quoted, buf) => SchemeString.Str(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Primitive.IsPrimitive(obj),    (x, quoted, buf) => Primitive.AsPrimitive(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Continuation.IsContinuation(obj), (x, quoted, buf) => Continuation.AsContinuation(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Macro.IsMacro(obj),        (x, quoted, buf) => Macro.AsMacro(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Closure.IsClosure(obj),      (x, quoted, buf) => Closure.AsClosure(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Procedure.IsProcedure(obj),    (x, quoted, buf) => Procedure.AsProcedure(x).AsString(quoted, buf)),
                new AsStringEntry(obj => InputPort.IsInputPort(obj),    (x, quoted, buf) => InputPort.AsInputPort(x).AsString(quoted, buf)),
                new AsStringEntry(obj => OutputPort.IsOutputPort(obj),   (x, quoted, buf) => OutputPort.AsOutputPort(x).AsString(quoted, buf)),
                new AsStringEntry(obj => EmptyList.IsEmptyList(obj),    (x, quoted, buf) => EmptyList.AsEmptyList(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Stepper.IsStepper(obj),      (x, quoted, buf) => Stepper.AsStepper(x).AsString(quoted, buf)),
                new AsStringEntry(obj => Undefined.IsUndefined(obj),    (x, quoted, buf) => Undefined.AsUndefined(x).AsString(quoted, buf)),
            };
        #endregion

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
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        internal static void AsString(Obj x, bool quoted, StringBuilder buf)
        {
            // see if we can find it in the table
            foreach (var entry in asStringEntries)
            {
                if (entry.TypePredicate(x))
                {
                    entry.AsStringFun(x, quoted, buf);
                    return;
                }
            }

            // use the built-in ToString if not in the table
            buf.Append(x);    
        }
        #endregion

        #region Print Table Struct
        /// <summary>
        /// Table entry for type name mapper.
        /// </summary>
        private struct AsStringEntry
        {
            /// <summary>
            /// The type tester for this entry.
            /// </summary>
            public readonly Func<object, bool> TypePredicate;

            /// <summary>
            /// The function that gets the type name.
            /// </summary>
            public readonly Action<object, bool, StringBuilder> AsStringFun;

            /// <summary>
            /// Initializes a new instance of the Printer.AsStringEntry struct.
            /// </summary>
            /// <param name="typePredicate">Type tester.</param>
            /// <param name="asStringFun">Type string generator.</param>
            public AsStringEntry(Func<object, bool> typePredicate, Action<object, bool, StringBuilder> asStringFun)
            {
                this.TypePredicate = typePredicate;
                this.AsStringFun = asStringFun;
            }
        }
        #endregion
    }
}