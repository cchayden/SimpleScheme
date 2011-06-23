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
    /// </summary>
    public class SchemeString : SchemeUtils, IEnumerable<char>
    {
        /// <summary>
        /// The characters making up the string.
        /// </summary>
        private string str;

        /// <summary>
        /// Initializes a new instance of the SchemeString class.
        /// </summary>
        /// <param name="str">The characters that make up the string.</param>
        public SchemeString(string str)
        {
            this.str = str;
        }

        /// <summary>
        /// Gets the string length.
        /// </summary>
        public new int Length
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
            for (int i = 0; i < xstr.str.Length; i++)
            {
                int diff = !ci
                               ? xstr.str[i] - ystr.str[i]
                               : char.ToLower(xstr.str[i]) - char.ToLower(ystr.str[i]);
                if (diff != 0)
                {
                    return diff;
                }
            }

            return xstr.str.Length - ystr.str.Length;
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

            for (int i = xstr.str.Length - 1; i >= 0; i--)
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

            return Str(Error("Expected a string, got: " + x));
        }

        /// <summary>
        /// Convert a list of chars into a string.
        /// </summary>
        /// <param name="chars">The object that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        public static SchemeString ListToString(object chars)
        {
            char[] str = new char[SchemeUtils.Length(chars)];

            int i = 0;
            while (chars is Pair)
            {
                str[i] = Chr(First(chars));
                chars = Rest(chars);
                i++;
            }

            return new SchemeString(new string(str));
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
                result = Cons(Chr(str[i]), result);
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
            while (args is Pair)
            {
                result.Append(AsString(First(args), false));
                args = Rest(args);
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
                return SchemeString.Compare((SchemeString)x, (SchemeString)y, ci);
            }

            Error("StringCompare: expected two strings, got: " + AsString(List(x, y)));
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
            int numberBase = y is double ? (int)NumberUtils.Num(y) : 10;
            try
            {
                return numberBase == 10
                           ? double.Parse(AsString(x, false))
                           : NumberUtils.Num(Convert.ToInt64(AsString(x, false), numberBase));
            }
            catch (FormatException)
            {
                return False;
            }
            catch (ArgumentException)
            {
                return False;
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
                for (int i = 0; i < v.Length; i++)
                {
                    AsString(v[i], quoted, buf);
                    if (i != v.Length - 1)
                    {
                        buf.Append(' ');
                    }
                }

                buf.Append(')');
            }
            else if (IsTrue(x))
            {
                buf.Append("#t");
            }
            else if (IsFalse(x))
            {
                buf.Append("#f");
            }
            else
            {
                buf.Append(x);
            }
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