// <copyright file="StringUtils.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// String utilities used by the primitives.
    /// </summary>
    public class StringUtils : SchemeUtils
    {
        /// <summary>
        /// Turn an object (storing a string) into an array of characters.
        /// </summary>
        /// <param name="x">The string object.</param>
        /// <returns>The character array.</returns>
        public static char[] Str(object x)
        {
            try
            {
                return (char[])x;
            }
            catch (InvalidCastException)
            {
                return Str(Error("expected a string, got: " + x));
            }
        }

        /// <summary>
        /// Convert a list of chars into a string.
        /// </summary>
        /// <param name="chars">The object that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        public static char[] ListToString(object chars)
        {
            char[] str = new char[Length(chars)];

            int i = 0;
            while (chars is Pair)
            {
                str[i] = Chr(First(chars));
                chars = Rest(chars);
                i++;
            }

            return str;
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        public static char[] StringAppend(object args)
        {
            StringBuilder result = new StringBuilder();
            while (args is Pair)
            {
                result.Append(AsString(First(args), false));
                args = Rest(args);
            }

            return result.ToString().ToCharArray();
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
            if (x is char[] && y is char[])
            {
                char[] xc = (char[])x;
                char[] yc = (char[])y;
                for (int i = 0; i < xc.Length; i++)
                {
                    int diff = !ci ? 
                        xc[i] - yc[i] : 
                        char.ToLower(xc[i]) - char.ToLower(yc[i]);
                    if (diff != 0)
                    {
                        return diff;
                    }
                }

                return xc.Length - yc.Length;
            }

            Error("expected two strings, got: " + AsString(List(x, y)));
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
            else if (x is char[])
            {
                char[] chars = (char[])x;
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
            else if (x is object[])
            {
                object[] v = (object[])x;
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
    }
}
