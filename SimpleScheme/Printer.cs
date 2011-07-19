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
                new AsStringEntry(obj => TypePrimitives.IsBoolean(obj), (x, quoted, buf) => BooleanAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsSymbol(obj), (x, quoted, buf) => SymbolAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsCharacter(obj), (x, quoted, buf) => CharacterAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsVector(obj), (x, quoted, buf) => VectorAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsPair(obj), (x, quoted, buf) => PairAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsNumber(obj), (x, quoted, buf) => NumberAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsString(obj), (x, quoted, buf) => StringAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsProcedure(obj), (x, quoted, buf) => ProcedureAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsInputPort(obj), (x, quoted, buf) => InputPortAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsOutputPort(obj), (x, quoted, buf) => OutputPortAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsEmptyList(obj), (x, quoted, buf) => EmptyListAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsStepper(obj), (x, quoted, buf) => StepperAsString(x, quoted, buf)),
                new AsStringEntry(obj => TypePrimitives.IsUndefined(obj), (x, quoted, buf) => UndefinedAsString(x, quoted, buf)),
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
            foreach (var entry in asStringEntries)
            {
                if (entry.TypePredicate(x))
                {
                    entry.AsStringFun(x, quoted, buf);
                    return;
                }
            }

            buf.Append(x);
        }
        #endregion

        #region Private Static Methods

        /// <summary>
        /// Convert the bool instance to a string.
        /// </summary>
        /// <param name="obj">The boolean value to convert.</param>
        /// <param name="quoted">True if the string should be quoted.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        private static void BooleanAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            buf.Append((bool)obj ? "#t" : "#f");
        }

        /// <summary>
        /// Convert symbol into string.
        /// </summary>
        /// <param name="obj">The symbol name.</param>
        /// <param name="quoted">Whether to quote the string.</param>
        /// <param name="buf">Accumulate the result into here.</param>
        private static void SymbolAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            buf.Append((string)obj);
        }

        /// <summary>
        /// Convert the character a string.
        /// </summary>
        /// <param name="obj">The character to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        private static void CharacterAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            char c = (char)obj;
            if (quoted)
            {
                buf.Append("#\\");
            }

            if (c == ' ')
            {
                buf.Append("space");
            }
            else
            {
                buf.Append(c);
            }
        }

        /// <summary>
        /// Convert the vector a string.
        /// </summary>
        /// <param name="obj">The vector to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        private static void VectorAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            Obj[] vec = (Obj[])obj;
            buf.Append("#(");
            if (vec.Length > 0)
            {
                foreach (Obj v in vec)
                {
                    AsString(v, quoted, buf);
                    buf.Append(' ');
                }

                buf.Remove(buf.Length - 1, 1);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Turns the pair into a string.
        /// Handle some special forms separately.
        /// Otherwise, just iterate down the list printing each element.
        /// Also, detect and handle improper lists.
        /// </summary>
        /// <param name="obj">The object to convert to a string.</param>
        /// <param name="quoted">Is the string to be quoted?</param>
        /// <param name="buf">The buffer to write the string into.</param>
        private static void PairAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            Pair pair = (Pair)obj;
            if (TypePrimitives.IsPair(ListPrimitives.Rest(pair)) && 
                TypePrimitives.IsEmptyList(ListPrimitives.Rest(ListPrimitives.Rest(pair))))
            {
                string special = null;

                // There is just one more thing in the pair.  See if the first thing 
                //    is one of these special forms.
                switch (ListPrimitives.First(pair) as string)
                {
                    case "quote":
                        special = "'";
                        break;
                    case "quasiquote":
                        special = "`";
                        break;
                    case "unquote":
                        special = ",";
                        break;
                    case "unquote-splicing":
                        special = ",@";
                        break;
                }

                if (special != null)
                {
                    // There was a special form, and one more thing.
                    // Append a special symbol and the remaining thing.
                    buf.Append(special);
                    AsString(ListPrimitives.Second(pair), quoted, buf);
                    return;
                }
            }

            // Normal case -- put out the whole list within parentheses.
            buf.Append('(');
            AsString(ListPrimitives.First(pair), quoted, buf);

            Obj tail = ListPrimitives.Rest(pair);

            int len = 0;
            while (TypePrimitives.IsPair(tail))
            {
                buf.Append(' ');
                AsString(ListPrimitives.First(tail), quoted, buf);
                Obj oldTail = tail;
                tail = ListPrimitives.Rest(tail);
                len++;
                if (tail == oldTail)
                {
                    // this is a circular structure -- truncate
                    buf.Append(" ... [circular list]");
                    tail = EmptyList.Instance;
                    break;
                }

                if (len > 1000)
                {
                    // maybe this is a circular structure -- truncate
                    buf.Append(" ... [too long]");
                    tail = EmptyList.Instance;
                    break;
                }
            }

            if (!TypePrimitives.IsEmptyList(tail))
            {
                buf.Append(" . ");
                AsString(tail, quoted, buf);
            }

            buf.Append(')');
        }

        /// <summary>
        /// Convert the number a string.
        /// </summary>
        /// <param name="obj">The number to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        private static void NumberAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            double d = Number.Num(obj);
            if (Math.Round(d) == d)
            {
                buf.Append((long)d);
            }
            else
            {
                buf.Append(d);
            }
        }

        /// <summary>
        /// Convert a scheme string into a string for output.
        /// </summary>
        /// <param name="obj">The string to output.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        private static void StringAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            char[] str = (char[])obj;
            if (! quoted)
            {
                buf.Append(str);
                return;
            }

            buf.Append('"');

            if (str != null)
            {
                foreach (char c in str)
                {
                    if (c == '"')
                    {
                        buf.Append('\\');
                    }

                    buf.Append(c);
                }
            }

            buf.Append('"');
        }

        /// <summary>
        /// Write the procedure to the string builder.
        /// </summary>
        /// <param name="obj">The procedure (not used).</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        private static void ProcedureAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            buf.Append("<procedure>");
        }

        /// <summary>
        /// Write the input port to a buffer.
        /// </summary>
        /// <param name="obj">The input port (not used).</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The buffer to write to.</param>
        private static void InputPortAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            buf.Append("<input port>");
        }

        /// <summary>
        /// Write the output port to the string builder.
        /// </summary>
        /// <param name="obj">The output port (not used).</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        private static void OutputPortAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            buf.Append("<output port>");
        }

        /// <summary>
        /// Write the empty list to the string builder.
        /// </summary>
        /// <param name="obj">The empty list (not used).</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The buffer to write into.</param>
        private static void EmptyListAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            buf.Append("()");
        }

        /// <summary>
        /// Convert the stepper instance to a string.
        /// If not quoted, then print nothing.
        /// Could also consider printing the Expr.
        /// </summary>
        /// <param name="obj">The stepper.</param>
        /// <param name="quoted">True if the string should be quoted.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        private static void StepperAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append(obj == Stepper.Suspended ? "<suspended>" : "<stepper>");
            }
        }

        /// <summary>
        /// Convert the undefined instance a string.
        /// If not quoted, then print nothing.
        /// </summary>
        /// <param name="obj">The undefined object.</param>
        /// <param name="quoted">True if the string should be quoted.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        private static void UndefinedAsString(Obj obj, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<undefined>");
            }
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