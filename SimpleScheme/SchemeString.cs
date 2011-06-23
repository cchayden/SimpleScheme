﻿// <copyright file="SchemeString.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// This represents scheme strings.
    /// They are represented as a character array.
    /// This file also contains character and symbol operations.
    /// </summary>
    public sealed class SchemeString : ListPrimitives
    {
        /// <summary>
        /// Creates a scheme string from a length and fill character.
        /// </summary>
        /// <param name="length">The length of the string to make.</param>
        /// <param name="fill">If present, the character to fill the string with.</param>
        /// <returns>The new scheme string.</returns>
        public static char[] MakeString(object length, object fill)
        {
            char c = (fill == List.Empty) ? (char)0 : Character.Chr(fill);
            return new string(c, (int)Number.Num(length)).ToCharArray();
        }

        /// <summary>
        /// Initializes a new instance of the SchemeString class.
        /// </summary>
        /// <param name="buf">A string builder containing the string value.</param>
        /// <returns>The new scheme string.</returns>
        public static char[] MakeString(StringBuilder buf)
        {
            return buf.ToString().ToCharArray();
        }

        /// <summary>
        /// Make a new copy of the given scheme string.
        /// </summary>
        /// <param name="str">The existing scheme string.</param>
        /// <returns>A copy of the scheme string.</returns>
        public static char[] MakeString(char[] str)
        {
            return new string(str).ToCharArray();
        }

        /// <summary>
        /// Make a (scheme) string from the .NET string.
        /// </summary>
        /// <param name="str">The string to convert.</param>
        /// <returns>The scheme string.</returns>
        public static char[] MakeString(string str)
        {
            return str.ToCharArray();
        }

        /// <summary>
        /// Get the length of a string.
        /// </summary>
        /// <param name="str">The string to measure.</param>
        /// <returns>The string length.</returns>
        public static int StringLength(char[] str)
        {
            return str.Length;
        }

        /// <summary>
        /// Define the string primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.7">(list->string <chars>)</r4rs>
                .DefinePrimitive("list->string", (caller, args) => ListToString(First(args)), 1)
                //// <r4rs section="6.7">(make-string <k>)</r4rs>
                //// <r4rs section="6.7">(make-string <k> <char>)</r4rs>
                .DefinePrimitive("make-string", (caller, args) => MakeString(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.7">(string <char> ...)</r4rs>
                .DefinePrimitive("string", (caller, args) => ListToString(args), 0, MaxInt)
                //// <r4rs section="6.7">(string->list <string>)</r4rs>
                .DefinePrimitive("string->list", (caller, args) => StringToList(First(args)), 1)
                //// <r4rs section="6.5.6">(string->number <number>)</r4rs>
                //// <r4rs section="6.5.6">(string->number <number> <radix>)</r4rs>
                .DefinePrimitive("string->number", (caller, args) => StringToNumber(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.4">(string->symbol <string>)</r4rs>
                .DefinePrimitive("string->symbol", (caller, args) => string.Intern(new string(Str(First(args)))), 1)
                //// <r4rs section="6.7">(string-append <string> ...)</r4rs>
                .DefinePrimitive("string-append", (caller, args) => StringAppend(args), 0, MaxInt)
                .DefinePrimitive("string-concat", (caller, args) => StringAppend(First(args)), 1)
                //// <r4rs section="6.7">(string-ci<=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci<=?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) <= 0), 2)
                //// <r4rs section="6.7">(string-ci<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci<?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) < 0), 2)
                //// <r4rs section="6.7">(string-ci=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci=?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) == 0), 2)
                //// <r4rs section="6.7">(string-ci>=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci>=?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) >= 0), 2)
                //// <r4rs section="6.7">(string-ci>? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci>?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), true) > 0), 2)
                //// <r4rs section="6.7">(string-copy <string>)</r4rs>
                .DefinePrimitive("string-copy", (caller, args) => StringCopy(First(args)), 1)
                //// <r4rs section="6.7">(string-fill! <string> <char>)</r4rs>
                .DefinePrimitive("string-fill!", (caller, args) => StringFill(First(args), Second(args)), 2)
                //// <r4rs section="6.7">(string-length <string>)</r4rs>
                .DefinePrimitive("string-length", (caller, args) => StringLength(Str(First(args))), 1)
                //// <r4rs section="6.7">(string-ref <string> <k>)</r4rs>
                .DefinePrimitive("string-ref", (caller, args) => Character.Chr(Str(First(args))[(int)Number.Num(Second(args))]), 2)
                //// <r4rs section="6.7">(string-set! <string> <k> <char>)</r4rs>
                .DefinePrimitive("string-set!", (caller, args) => StringSet(First(args), Second(args), Third(args)), 3)
                //// <r4rs section="6.7">(string<=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string<=?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) <= 0), 2)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string<?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) < 0), 2)
                //// <r4rs section="6.7">(string=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string=?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) == 0), 2)
                //// <r4rs section="6.7">(string>=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string>=?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) >= 0), 2)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string>?", (caller, args) => SchemeBoolean.Truth(StringCompare(First(args), Second(args), false) > 0), 2)
                //// <r4rs section="6.7">(string? <obj>)</r4rs>
                .DefinePrimitive("string?", (caller, args) => SchemeBoolean.Truth(First(args) is char[]), 1)
                //// <r4rs section="6.7">(substring <string> <start> <end>)</r4rs>
                .DefinePrimitive("substring", (caller, args) => Substr(First(args), Second(args), Third(args)), 3);
        }

        /// <summary>
        /// Tests two strings for equality.
        /// </summary>
        /// <param name="xstr">The first string.</param>
        /// <param name="ystr">The second string.</param>
        /// <returns>True if the strings are equal.</returns>
        public static bool Equal(char[] xstr, char[] ystr)
        {
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
        /// <param name="chars">The object that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        public static char[] ListToString(object chars)
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
            else if (x is object[])
            {
                Vector.AsString((object[])x, quoted, buf);
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
        /// Covert the SchemeString into a string.
        /// </summary>
        /// <param name="str">The string to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void AsString(char[] str, bool quoted, StringBuilder buf)
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

        /// <summary>
        /// Return the substring of a string.
        /// </summary>
        /// <param name="str">The original string.</param>
        /// <param name="start">The starting position.</param>
        /// <param name="end">The ending position.</param>
        /// <returns>The substring starting with the starting position and ending with the ending position.</returns>
        private static char[] Substr(object str, object start, object end)
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
        /// Turn an object (storing a scheme string) into an array of characters.
        /// </summary>
        /// <param name="x">The string object.</param>
        /// <returns>The character array.</returns>
        private static char[] Str(object x)
        {
            if (x is char[])
            {
                return (char[])x;
            }

            return Str(ErrorHandlers.Error("Expected a string, got: " + x));
        }

        /// <summary>
        /// Convert a string to a list of characters.
        /// </summary>
        /// <param name="s">The string to convert.</param>
        /// <returns>A list of the characters.</returns>
        private static object StringToList(object s)
        {
            object result = List.Empty;
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
        /// <returns>Undefined object.</returns>
        private static object StringSet(object str, object index, object chr)
        {
            Str(str)[(int)Number.Num(index)] = Character.Chr(chr);
            return Undefined.Instance;
        }

        /// <summary>
        /// Make a copy of the given string.
        /// </summary>
        /// <param name="str">The string to copy.</param>
        /// <returns>The return value is unspecified.</returns>
        private static object StringCopy(object str)
        {
            return MakeString(Str(str));
        }

        /// <summary>
        /// Update the string by filling it with the fill char.
        /// </summary>
        /// <param name="str">The string to fill.</param>
        /// <param name="fill">The fill character.</param>
        /// <returns>The return value is unspecified.</returns>
        private static object StringFill(object str, object fill)
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
        private static char[] StringAppend(object args)
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
        private static int StringCompare(object x, object y, bool ci)
        {
            if (x is char[] && y is char[])
            {
                return Compare((char[])x, (char[])y, ci);
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
        private static object StringToNumber(object x, object y)
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
    }
}