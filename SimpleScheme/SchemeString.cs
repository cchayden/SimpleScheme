// <copyright file="SchemeString.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Text;

    /// <summary>
    /// This represents scheme strings.
    /// They are represented internally as a character array.
    /// This also contains character and symbol operations.
    /// </summary>
    public sealed class SchemeString : IEnumerable<char>
    {
        /// <summary>
        /// The characters making up the string.
        /// </summary>
        private string str;

        /// <summary>
        /// Initializes a new instance of the SchemeString class.
        /// </summary>
        /// <param name="str">The (CLR)string that makes up the string.</param>
        public SchemeString(string str)
        {
            this.str = str;
        }

        /// <summary>
        /// Initializes a new instance of the SchemeString class.
        /// </summary>
        /// <param name="length">The length of the string to make.</param>
        /// <param name="fill">If present, the character to fill the string with.</param>
        public SchemeString(object length, object fill)
        {
            char c = (fill == null) ? (char)0 : Chr(fill);
            this.str = new string(c, (int)Number.Num(length));
        }

        /// <summary>
        /// Initializes a new instance of the SchemeString class.
        /// </summary>
        /// <param name="buf">A string builder containing the string value.</param>
        public SchemeString(StringBuilder buf)
        {
            this.str = buf.ToString();
        }

        /// <summary>
        /// Gets the string length.
        /// </summary>
        public int Length
        {
            get { return this.str.Length; }
        }

        /// <summary>
        /// Gets a character from the string.
        /// </summary>
        /// <param name="i">The index of the character in the string.</param>
        /// <returns>The indexed character.</returns>
        public char this[int i]
        {
            get
            {
                return this.str[i];
            }

            set
            {
                StringBuilder sb = new StringBuilder(this.str);
                sb[i] = value;
                this.str = sb.ToString();
            }
        }

        /// <summary>
        /// Compare two strings.
        /// </summary>
        /// <param name="xstr">The first string.</param>
        /// <param name="ystr">The second string.</param>
        /// <param name="ci">Case invariant flag.</param>
        /// <returns>Zero if the strings are the same, negative if the first is less, positive
        /// if the second is less.</returns>
        public static int Compare(SchemeString xstr, SchemeString ystr, bool ci)
        {
            int diff = xstr.str.Length - ystr.str.Length;
            if (diff != 0)
            {
                return diff;
            }

            int len = xstr.str.Length;
            for (int i = 0; i < len; i++)
            {
                diff = ci
                           ? char.ToLower(xstr.str[i]) - char.ToLower(ystr.str[i])
                           : xstr.str[i] - ystr.str[i];
                if (diff != 0)
                {
                    return diff;
                }
            }

            return 0;
        }

        /// <summary>
        /// Tests two strings for equality.
        /// </summary>
        /// <param name="xstr">The first string.</param>
        /// <param name="ystr">The second string.</param>
        /// <returns>True if the strings are equal.</returns>
        public static bool Equal(SchemeString xstr, SchemeString ystr)
        {
            if (xstr.str.Length != ystr.str.Length)
            {
                return false;
            }

            int len = xstr.str.Length;
            for (int i = 0; i < len; i++)
            {
                if (xstr.str[i] != ystr.str[i])
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Turn an object (storing a string) into an array of characters.
        /// </summary>
        /// <param name="x">The string object.</param>
        /// <returns>The character array.</returns>
        public static SchemeString Str(object x)
        {
            if (x is SchemeString)
            {
                return (SchemeString)x;
            }

            return Str(ErrorHandlers.Error("Expected a string, got: " + x));
        }

        /// <summary>
        /// Convert a list of chars into a string.
        /// </summary>
        /// <param name="chars">The object that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        public static SchemeString ListToString(object chars)
        {
            StringBuilder str = new StringBuilder();
            if (chars is Pair)
            {
                foreach (object elem in (Pair)chars)
                {
                    str.Append(Chr(elem));
                }
            }

            return new SchemeString(str.ToString());
        }

        /// <summary>
        /// Convert a string to a list of characters.
        /// </summary>
        /// <param name="s">The string to convert.</param>
        /// <returns>A list of the characters.</returns>
        public static object StringToList(object s)
        {
            Pair result = null;
            SchemeString str = Str(s);
            for (int i = str.Length - 1; i >= 0; i--)
            {
                result = List.Cons(Chr(str[i]), result);
            }

            return result;
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        public static SchemeString StringAppend(object args)
        {
            StringBuilder result = new StringBuilder();
            // TODO convert to user foreach
            while (args is Pair)
            {
                result.Append(AsString(List.First(args), false));
                args = List.Rest(args);
            }

            return new SchemeString(result.ToString());
        }

        /// <summary>
        /// Compare two strings.  Comparison may be case insensitive.
        /// Return value indicating their relative order.
        /// </summary>
        /// <param name="x">The first string.</param>
        /// <param name="y">The second string.</param>
        /// <param name="ci">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if first string less then second, zero if they are equal, 
        /// positive if first is greater.</returns>
        public static int StringCompare(object x, object y, bool ci)
        {
            if (x is SchemeString && y is SchemeString)
            {
                return Compare((SchemeString)x, (SchemeString)y, ci);
            }

            ErrorHandlers.Error("StringCompare: expected two strings, got: " + AsString(x) + " and " + AsString(y));
            return 0;
        }

        /// <summary>
        /// Convert a string into a number, in a given number base.
        /// </summary>
        /// <param name="x">The value to convert.  This is first converted to a string, 
        ///     then parsed as a number.</param>
        /// <param name="y">The number base.  If not a number, then base 10 is used.</param>
        /// <returns>The number represented by the string.</returns>
        public static object StringToNumber(object x, object y)
        {
            int numberBase = y is double ? (int)Number.Num(y) : 10;
            try
            {
                return numberBase == 10
                           ? double.Parse(AsString(x, false))
                           : Number.Num(Convert.ToInt64(AsString(x, false), numberBase));
            }
            catch (FormatException)
            {
                return SchemeBoolean.False;
            }
            catch (ArgumentException)
            {
                return SchemeBoolean.False;
            }
        }

        /// <summary>
        /// Convert an object into a string representation.
        /// </summary>
        /// <param name="x">The object to convert.</param>
        /// <returns>The string representing the object.</returns>
        public static string AsString(object x)
        {
            return AsString(x, true);
        }

        /// <summary>
        /// Convert an object into a string representation.
        /// </summary>
        /// <param name="x">The object to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the object.</returns>
        public static string AsString(object x, bool quoted)
        {
            StringBuilder buf = new StringBuilder();
            AsString(x, quoted, buf);
            return buf.ToString();
        }

        /// <summary>
        /// Convert an object into a string representation.
        /// </summary>
        /// <param name="x">The object to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void AsString(object x, bool quoted, StringBuilder buf)
        {
            if (x == null)
            {
                buf.Append("()");
            }
            else if (x is double)
            {
                double d = (double)x;
                if (Math.Round(d) == d)
                {
                    buf.Append((long)d);
                }
                else
                {
                    buf.Append(d);
                }
            }
            else if (x is char)
            {
                if (quoted)
                {
                    buf.Append("#\\");
                }

                buf.Append(x);
            }
            else if (x is Pair)
            {
                ((Pair)x).AsString(quoted, buf);
            }
            else if (x is SchemeString)
            {
                SchemeString chars = (SchemeString)x;
                if (quoted)
                {
                    buf.Append('"');
                }

                foreach (char c in chars)
                {
                    if (quoted && c == '"')
                    {
                        buf.Append('\\');
                    }

                    buf.Append(c);
                }

                if (quoted)
                {
                    buf.Append('"');
                }
            }
            else if (x is Vector)
            {
                Vector v = (Vector)x;
                buf.Append("#(");
                if (v.Length > 0)
                {
                    foreach (object elem in v)
                    {
                        AsString(elem, quoted, buf);
                        buf.Append(' ');
                    }

                    buf.Remove(buf.Length - 1, 1);
                }

                buf.Append(')');
            }
            else if (x is Stepper)
            {
                Stepper s = (Stepper)x;
                buf.Append(s.Expr);
            }
            else if (SchemeBoolean.IsTrue(x))
            {
                buf.Append("#t");
            }
            else if (SchemeBoolean.IsFalse(x))
            {
                buf.Append("#f");
            }
            else
            {
                buf.Append(x);
            }
        }

        // Character operations

        /// <summary>
        /// Convert an object containing a character into the character.
        /// </summary>
        /// <param name="x">The object containing the char.</param>
        /// <returns>The character it contains.</returns>
        public static char Chr(object x)
        {
            if (!(x is char))
            {
                return Chr(ErrorHandlers.Error("Expected a char, got: " + x));
            }

            return (char)x;
        }

        /// <summary>
        /// Compares two characters.
        /// </summary>
        /// <param name="x">The first char.</param>
        /// <param name="y">The second char.</param>
        /// <param name="ci">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if x is before y, positive if x is after y, 
        /// or 0 if they are the same.</returns>
        public static int ChrCompare(object x, object y, bool ci)
        {
            char xc = Chr(x);
            char yc = Chr(y);
            if (ci)
            {
                xc = char.ToLower(xc);
                yc = char.ToLower(yc);
            }

            return xc - yc;
        }

        // Symbol operations

        /// <summary>
        /// Turn an object that is a symbol into a string.
        /// It is stored as one already, so just verify that this is a symbol.
        /// </summary>
        /// <param name="x">The symbol.</param>
        /// <returns>The corresponding string.</returns>
        public static string Sym(object x)
        {
            if (x is string)
            {
                return (string)x;
            }

            return Sym(ErrorHandlers.Error("Expected a symbol, got: " + x));
        }

        /// <summary>
        /// Gets the SchemeString as a string.
        /// </summary>
        /// <returns>The contained string.</returns>
        public string AsString()
        {
            return this.str;
        }

        /// <summary>
        /// Enumerates characters from the string.
        /// </summary>
        /// <returns>The string characters.</returns>
        public IEnumerator<char> GetEnumerator()
        {
            foreach (char c in this.str)
            {
                yield return c;
            }
        }

        /// <summary>
        /// Gets the string enumerator.
        /// </summary>
        /// <returns>The string enumerator.</returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        /// <summary>
        /// Get a substring from the string.
        /// </summary>
        /// <param name="start">The starting character position.</param>
        /// <param name="len">The number of characters.</param>
        /// <returns>A substring, starting at the given position.</returns>
        public SchemeString Substring(int start, int len)
        {
            return new SchemeString(this.str.Substring(start, len));
        }
    }
}