// <copyright file="SchemeString.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Handles scheme strings.
    /// Strings are represented as a character array.
    /// </summary>
    public sealed class SchemeString : ListPrimitives
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
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void AsString(Obj x, bool quoted, StringBuilder buf)
        {
            if (x == List.Empty)
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

                if ((char)x == ' ')
                {
                    buf.Append("space");
                } 
                else
                {
                    buf.Append(x);
                }
            }
            else if (x is Pair)
            {
                ((Pair)x).AsString(quoted, buf);
            }
            else if (x is string)
            {
                Symbol.AsString((string)x, quoted, buf);
            }
            else if (x is char[])
            {
                AsString((char[])x, quoted, buf);
            }
            else if (x is Obj[])
            {
                Vector.AsString((Obj[])x, quoted, buf);
            }
            else if (x is bool)
            {
                SchemeBoolean.AsString((bool)x, quoted, buf);
            }
            else if (x is Stepper)
            {
                ((Stepper)x).AsString(quoted, buf);
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

        /// <summary>
        /// Check that an oject is a scheme string.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The scheme string.</returns>
        public static char[] Str(Obj obj)
        {
            if (obj is char[])
            {
                return (char[])obj;
            }

            return Str(ErrorHandlers.TypeError("string", obj));
        }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the string primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.7">(list->string <chars>)</r4rs>
                .DefinePrimitive("list->string", (args, caller) => ListToString(First(args)), 1)
                //// <r4rs section="6.7">(make-string <k>)</r4rs>
                //// <r4rs section="6.7">(make-string <k> <char>)</r4rs>
                .DefinePrimitive("make-string", (args, caller) => MakeString(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.7">(string <char> ...)</r4rs>
                .DefinePrimitive("string", (args, caller) => ListToString(args), 0, MaxInt)
                //// <r4rs section="6.7">(string->list <string>)</r4rs>
                .DefinePrimitive("string->list", (args, caller) => StringToList(First(args)), 1)
                //// <r4rs section="6.5.6">(string->number <number>)</r4rs>
                //// <r4rs section="6.5.6">(string->number <number> <radix>)</r4rs>
                .DefinePrimitive("string->number", (args, caller) => StringToNumber(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.4">(string->symbol <string>)</r4rs>
                .DefinePrimitive("string->symbol", (args, caller) => string.Intern(new string(Str(First(args)))), 1)
                //// <r4rs section="6.7">(string-append <string> ...)</r4rs>
                .DefinePrimitive("string-append", (args, caller) => StringAppend(args), 0, MaxInt)
                .DefinePrimitive("string-concat", (args, caller) => StringAppend(First(args)), 1)
                //// <r4rs section="6.7">(string-ci<=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci<=?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) <= 0), 2)
                //// <r4rs section="6.7">(string-ci<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci<?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) < 0), 2)
                //// <r4rs section="6.7">(string-ci=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci=?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) == 0), 2)
                //// <r4rs section="6.7">(string-ci>=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci>=?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) >= 0), 2)
                //// <r4rs section="6.7">(string-ci>? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci>?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) > 0), 2)
                //// <r4rs section="6.7">(string-copy <string>)</r4rs>
                .DefinePrimitive("string-copy", (args, caller) => StringCopy(First(args)), 1)
                //// <r4rs section="6.7">(string-fill! <string> <char>)</r4rs>
                .DefinePrimitive("string-fill!", (args, caller) => StringFill(First(args), Second(args)), 2)
                //// <r4rs section="6.7">(string-length <string>)</r4rs>
                .DefinePrimitive("string-length", (args, caller) => StringLength(Str(First(args))), 1)
                //// <r4rs section="6.7">(string-ref <string> <k>)</r4rs>
                .DefinePrimitive("string-ref", (args, caller) => Character.Chr(Str(First(args))[(int)Number.Num(Second(args))]), 2)
                //// <r4rs section="6.7">(string-set! <string> <k> <char>)</r4rs>
                .DefinePrimitive("string-set!", (args, caller) => StringSet(First(args), Second(args), Third(args)), 3)
                //// <r4rs section="6.7">(string<=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string<=?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) <= 0), 2)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string<?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) < 0), 2)
                //// <r4rs section="6.7">(string=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string=?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) == 0), 2)
                //// <r4rs section="6.7">(string>=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string>=?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) >= 0), 2)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string>?", (args, caller) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) > 0), 2)
                //// <r4rs section="6.7">(string? <obj>)</r4rs>
                .DefinePrimitive("string?", (args, caller) => SchemeBoolean.Truth(First(args) is char[]), 1)
                //// <r4rs section="6.7">(substring <string> <start> <end>)</r4rs>
                .DefinePrimitive("substring", (args, caller) => Substr(First(args), Second(args), Third(args)), 3);
        }
        #endregion

        #region Internal Static Methods

        /// <summary>
        /// Creates a scheme string from a length and fill character.
        /// </summary>
        /// <param name="length">The length of the string to make.</param>
        /// <param name="fill">If present, the character to fill the string with.</param>
        /// <returns>The new scheme string.</returns>
        internal static char[] MakeString(Obj length, Obj fill)
        {
            char c = (fill == List.Empty) ? (char)0 : Character.Chr(fill);
            return new string(c, (int)Number.Num(length)).ToCharArray();
        }

        /// <summary>
        /// Initializes a new instance of the SchemeString class.
        /// </summary>
        /// <param name="buf">A string builder containing the string value.</param>
        /// <returns>The new scheme string.</returns>
        internal static char[] MakeString(StringBuilder buf)
        {
            return buf.ToString().ToCharArray();
        }

        /// <summary>
        /// Make a new copy of the given scheme string.
        /// </summary>
        /// <param name="str">The existing scheme string.</param>
        /// <returns>A copy of the scheme string.</returns>
        internal static char[] MakeString(char[] str)
        {
            return new string(str).ToCharArray();
        }

        /// <summary>
        /// Make a (scheme) string from the .NET string.
        /// </summary>
        /// <param name="str">The string to convert.</param>
        /// <returns>The scheme string.</returns>
        internal static char[] MakeString(string str)
        {
            return str.ToCharArray();
        }

        /// <summary>
        /// Get the length of a string.
        /// </summary>
        /// <param name="str">The string to measure.</param>
        /// <returns>The string length.</returns>
        internal static int StringLength(char[] str)
        {
            return str.Length;
        }

        /// <summary>
        /// Tests two strings for equality.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme string..</param>
        /// <param name="obj2">The second object.</param>
        /// <returns>True if the strings are equal.</returns>
        internal static bool Equal(Obj obj1, Obj obj2)
        {
            if (!(obj2 is char[]))
            {
                return false;
            }

            char[] xstr = (char[])obj1;
            char[] ystr = (char[])obj2;

            if (xstr.Length != ystr.Length)
            {
                return false;
            }

            int len = xstr.Length;
            for (int i = 0; i < len; i++)
            {
                if (xstr[i] != ystr[i])
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Convert a list of chars into a string.
        /// </summary>
        /// <param name="chars">The obj that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        internal static char[] ListToString(Obj chars)
        {
            StringBuilder str = new StringBuilder();
            while (chars is Pair)
            {
                str.Append(Character.Chr(First(chars)));
                chars = Rest(chars);
            }

            return MakeString(str);
        }

        /// <summary>
        /// Covert the SchemeString into a string.
        /// </summary>
        /// <param name="str">The string to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        internal static void AsString(char[] str, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append('"');
            }

            if (str != null)
            {
                foreach (char c in str)
                {
                    if (quoted && c == '"')
                    {
                        buf.Append('\\');
                    }

                    buf.Append(c);
                }
            }

            if (quoted)
            {
                buf.Append('"');
            }
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Return the substring of a string.
        /// </summary>
        /// <param name="str">The original string.</param>
        /// <param name="start">The starting position.</param>
        /// <param name="end">The ending position.</param>
        /// <returns>The substring starting with the starting position and ending with the ending position.</returns>
        private static char[] Substr(Obj str, Obj start, Obj end)
        {
            int startPos = (int)Number.Num(start);
            int endPos = (int)Number.Num(end);
            int len = endPos - startPos;
            char[] newStr = new char[len];
            for (int i = 0; i < len; i++)
            {
                newStr[i] = Str(str)[startPos + i];
            }

            return newStr;
        }

        /// <summary>
        /// Compare two strings.
        /// </summary>
        /// <param name="xstr">The first string.</param>
        /// <param name="ystr">The second string.</param>
        /// <param name="ci">Case invariant flag.</param>
        /// <returns>Zero if the strings are the same, negative if the first is less, positive
        /// if the second is less.</returns>
        private static int Compare(char[] xstr, char[] ystr, bool ci)
        {
            int diff = xstr.Length - ystr.Length;
            if (diff != 0)
            {
                return diff;
            }

            int len = xstr.Length;
            for (int i = 0; i < len; i++)
            {
                diff = ci
                           ? char.ToLower(xstr[i]) - char.ToLower(ystr[i])
                           : xstr[i] - ystr[i];
                if (diff != 0)
                {
                    return diff;
                }
            }

            return 0;
        }

        /// <summary>
        /// Convert a string to a list of characters.
        /// </summary>
        /// <param name="s">The string to convert.</param>
        /// <returns>A list of the characters.</returns>
        private static Obj StringToList(object s)
        {
            Obj result = List.Empty;
            char[] str = Str(s);
            for (int i = str.Length - 1; i >= 0; i--)
            {
                result = Cons(Character.Chr(str[i]), result);
            }

            return result;
        }

        /// <summary>
        /// Assign a character in the string to a new character.
        /// </summary>
        /// <param name="str">The scheme string to modify.</param>
        /// <param name="index">The index of the character to change.</param>
        /// <param name="chr">The new character.</param>
        /// <returns>Undefined value.</returns>
        private static Obj StringSet(object str, object index, object chr)
        {
            Str(str)[(int)Number.Num(index)] = Character.Chr(chr);
            return Undefined.Instance;
        }

        /// <summary>
        /// Make a copy of the given string.
        /// </summary>
        /// <param name="str">The string to copy.</param>
        /// <returns>The return value is unspecified.</returns>
        private static Obj StringCopy(object str)
        {
            return MakeString(Str(str));
        }

        /// <summary>
        /// Update the string by filling it with the fill char.
        /// </summary>
        /// <param name="str">The string to fill.</param>
        /// <param name="fill">The fill character.</param>
        /// <returns>The return value is unspecified.</returns>
        private static Obj StringFill(object str, object fill)
        {
            char[] ss = Str(str);
            for (int i = 0; i < ss.Length; i++)
            {
                ss[i] = Character.Chr(fill);
            }

            return Undefined.Instance;
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        private static char[] StringAppend(Obj args)
        {
            StringBuilder result = new StringBuilder();

            while (args is Pair)
            {
                result.Append(AsString(First(args), false));
                args = Rest(args);
            }

            return MakeString(result);
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
        private static int StringCompare(Obj x, Obj y, bool ci)
        {
            if (x is char[] && y is char[])
            {
                return Compare((char[])x, (char[])y, ci);
            }

            ErrorHandlers.SemanticError("StringCompare: expected two strings, got: " + AsString(x) + " and " + AsString(y));
            return 0;
        }

        /// <summary>
        /// Convert a string into a number, in a given number base.
        /// </summary>
        /// <param name="x">The value to convert.  This is first converted to a string, 
        ///     then parsed as a number.</param>
        /// <param name="y">The number base.  If not a number, then base 10 is used.</param>
        /// <returns>The number represented by the string.</returns>
        private static Obj StringToNumber(object x, object y)
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
        #endregion
    }
}