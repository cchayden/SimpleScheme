// <copyright file="StringUtils.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    class StringUtils : SchemeUtils
    {
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
                result.Append(Stringify(First(args), false));
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

            Error("expected two strings, got: " + Stringify(List(x, y)));
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
            int numberBase = y is double ? (int)Num(y) : 10;
            try
            {
                return numberBase == 10
                           ? double.Parse(Stringify(x, false))
                           : Num(Convert.ToInt64(Stringify(x, false), numberBase));
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
    }
}
