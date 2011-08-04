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
    public static class SchemeString
    {
        #region Constants
        /// <summary>
        /// The printable name of the scheme string type.
        /// </summary>
        public const string Name = "string";
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Write the string to the string builder.
        /// </summary>
        /// <param name="str">The string.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(char[] str, bool quoted, StringBuilder buf)
        {
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
        /// Tests whether to given object is a scheme string.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme string.</returns>
        public static bool Is(Obj obj)
        {
            return obj is char[];
        }

        /// <summary>
        /// Check that an oject is a scheme string.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The scheme string.</returns>
        public static char[] As(Obj obj)
        {
            if (Is(obj))
            {
                return (char[])obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return null;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the string primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = Int32.MaxValue;
            env
                //// <r4rs section="6.7">(list->string <chars>)</r4rs>
                .DefinePrimitive("list->string", (args, caller) => ListToString(List.First(args)), 1)
                //// <r4rs section="6.7">(make-string <k>)</r4rs>
                //// <r4rs section="6.7">(make-string <k> <char>)</r4rs>
                .DefinePrimitive("make-string", (args, caller) => New(List.First(args), List.Second(args)), 1, 2)
                //// <r4rs section="6.7">(string <char> ...)</r4rs>
                .DefinePrimitive("string", (args, caller) => ListToString(args), 0, MaxInt)
                //// <r4rs section="6.7">(string->list <string>)</r4rs>
                .DefinePrimitive("string->list", (args, caller) => ToList(List.First(args)), 1)
                //// <r4rs section="6.5.6">(string->number <number>)</r4rs>
                //// <r4rs section="6.5.6">(string->number <number> <radix>)</r4rs>
                .DefinePrimitive("string->number", (args, caller) => ToNumber(List.First(args), List.Second(args)), 1, 2)
                //// <r4rs section="6.7">(string-append <string> ...)</r4rs>
                .DefinePrimitive("string-append", (args, caller) => Append(args), 0, MaxInt)
                .DefinePrimitive("string-concat", (args, caller) => Append(List.First(args)), 1)
                //// <r4rs section="6.7">(string-ci<=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci<=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) <= 0), 2)
                //// <r4rs section="6.7">(string-ci<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci<?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) < 0), 2)
                //// <r4rs section="6.7">(string-ci=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) == 0), 2)
                //// <r4rs section="6.7">(string-ci>=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci>=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) >= 0), 2)
                //// <r4rs section="6.7">(string-ci>? <string1> <string2>)</r4rs>
                .DefinePrimitive("string-ci>?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) > 0), 2)
                //// <r4rs section="6.7">(string-copy <string>)</r4rs>
                .DefinePrimitive("string-copy", (args, caller) => Copy(List.First(args)), 1)
                //// <r4rs section="6.7">(string-fill! <string> <char>)</r4rs>
                .DefinePrimitive("string-fill!", (args, caller) => Fill(List.First(args), List.Second(args)), 2)
                //// <r4rs section="6.7">(string-length <string>)</r4rs>
                .DefinePrimitive("string-length", (args, caller) => Length(As(List.First(args))), 1)
                //// <r4rs section="6.7">(string-ref <string> <k>)</r4rs>
                .DefinePrimitive("string-ref", (args, caller) => Character.As(As(List.First(args))[(int)Number.As(List.Second(args))]), 2)
                //// <r4rs section="6.7">(string-set! <string> <k> <char>)</r4rs>
                .DefinePrimitive("string-set!", (args, caller) => Set(List.First(args), List.Second(args), List.Third(args)), 3)
                //// <r4rs section="6.7">(string<=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string<=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) <= 0), 2)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string<?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) < 0), 2)
                //// <r4rs section="6.7">(string=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) == 0), 2)
                //// <r4rs section="6.7">(string>=? <string1> <string2>)</r4rs>
                .DefinePrimitive("string>=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) >= 0), 2)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive("string>?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) > 0), 2)
                //// <r4rs section="6.7">(string? <obj>)</r4rs>
                .DefinePrimitive("string?", (args, caller) => SchemeBoolean.Truth(Is(List.First(args))), 1)
                //// <r4rs section="6.7">(substring <string> <start> <end>)</r4rs>
                .DefinePrimitive("substring", (args, caller) => Substr(List.First(args), List.Second(args), List.Third(args)), 3)
                //// <r4rs section="6.4">(symbol->string <symbol>)</r4rs>
                .DefinePrimitive("symbol->string", (args, caller) => New(List.First(args)), 1);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates a scheme string from a string builder.
        /// </summary>
        /// <param name="buf">A string builder containing the string value.</param>
        /// <returns>The new scheme string.</returns>
        public static char[] New(StringBuilder buf)
        {
            return buf.ToString().ToCharArray();
        }

        /// <summary>
        /// Make a scheme string from the given object.
        /// The object must be a symbol
        /// </summary>
        /// <param name="str">The symbol to convert.</param>
        /// <returns>The scheme string.</returns>
        public static char[] New(Obj str)
        {
            return Symbol.As(str).ToCharArray();
        }

        /// <summary>
        /// Tests two strings for equality.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme string..</param>
        /// <param name="obj2">The second object.</param>
        /// <returns>True if the strings are equal.</returns>
        public static bool Equal(Obj obj1, Obj obj2)
        {
            if (!Is(obj2))
            {
                return false;
            }

            char[] str1 = (char[])obj1;
            char[] str2 = (char[])obj2;
            int len1 = str1.Length;
            int len2 = str2.Length;

            if (len1 != len2)
            {
                return false;
            }

            for (int i = 0; i < len1; i++)
            {
                if (str1[i] != str2[i])
                {
                    return false;
                }
            }

            return true;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Creates a scheme string from a length and fill character.
        /// </summary>
        /// <param name="length">The length of the string to make.</param>
        /// <param name="fill">If present, the character to fill the string with.</param>
        /// <returns>The new scheme string.</returns>
        private static char[] New(Obj length, Obj fill)
        {
            char c = EmptyList.Is(fill) ? (char)0 : Character.As(fill);
            return new string(c, (int)Number.As(length)).ToCharArray();
        }

        /// <summary>
        /// Make a new copy of the given scheme string.
        /// </summary>
        /// <param name="str">The existing scheme string.</param>
        /// <returns>A copy of the scheme string.</returns>
        private static char[] New(char[] str)
        {
            return new string(str).ToCharArray();
        }

        /// <summary>
        /// Get the length of a string.
        /// </summary>
        /// <param name="str">The string to measure.</param>
        /// <returns>The string length.</returns>
        private static int Length(char[] str)
        {
            return str.Length;
        }

        /// <summary>
        /// Convert a list of chars into a string.
        /// </summary>
        /// <param name="chars">The obj that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        private static char[] ListToString(Obj chars)
        {
            StringBuilder str = new StringBuilder();
            while (Pair.Is(chars))
            {
                str.Append(Character.As(List.First(chars)));
                chars = List.Rest(chars);
            }

            return New(str);
        }

        /// <summary>
        /// Return the substring of a string.
        /// </summary>
        /// <param name="str">The original string.</param>
        /// <param name="start">The starting position.</param>
        /// <param name="end">The ending position.</param>
        /// <returns>The substring starting with the starting position and ending with the ending position.</returns>
        private static char[] Substr(Obj str, Obj start, Obj end)
        {
            int startPos = (int)Number.As(start);
            int endPos = (int)Number.As(end);
            int len = endPos - startPos;
            char[] newStr = new char[len];
            for (int i = 0; i < len; i++)
            {
                newStr[i] = As(str)[startPos + i];
            }

            return newStr;
        }

        /// <summary>
        /// Convert a string to a list of characters.
        /// </summary>
        /// <param name="s">The string to convert.</param>
        /// <returns>A list of the characters.</returns>
        private static Obj ToList(object s)
        {
            Obj result = EmptyList.Instance;
            char[] str = As(s);
            for (int i = str.Length - 1; i >= 0; i--)
            {
                result = Pair.Cons(Character.As(str[i]), result);
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
        private static Obj Set(object str, object index, object chr)
        {
            As(str)[(int)Number.As(index)] = Character.As(chr);
            return Undefined.Instance;
        }

        /// <summary>
        /// Make a copy of the given string.
        /// </summary>
        /// <param name="str">The string to copy.</param>
        /// <returns>The return value is unspecified.</returns>
        private static Obj Copy(object str)
        {
            return New(As(str));
        }

        /// <summary>
        /// Update the string by filling it with the fill char.
        /// </summary>
        /// <param name="str">The string to fill.</param>
        /// <param name="fill">The fill character.</param>
        /// <returns>The return value is unspecified.</returns>
        private static Obj Fill(object str, object fill)
        {
            char[] ss = As(str);
            for (int i = 0; i < ss.Length; i++)
            {
                ss[i] = Character.As(fill);
            }

            return Undefined.Instance;
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        private static char[] Append(Obj args)
        {
            StringBuilder result = new StringBuilder();

            while (Pair.Is(args))
            {
                result.Append(Printer.AsString(List.First(args), false));
                args = List.Rest(args);
            }

            return New(result);
        }

        /// <summary>
        /// Compare two strings.  Comparison may be case insensitive.
        /// Return value indicating their relative order.
        /// </summary>
        /// <param name="x">The first string.</param>
        /// <param name="y">The second string.</param>
        /// <param name="caseInsensitive">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if first string less then second, zero if they are equal, 
        /// positive if first is greater.</returns>
        private static int Compare(Obj x, Obj y, bool caseInsensitive)
        {
            if (Is(x) && Is(y))
            {
                return Compare((char[])x, (char[])y, caseInsensitive);
            }

            ErrorHandlers.SemanticError("String.Compare: expected two strings, got: " + 
                Printer.AsString(x) + " and " + 
                Printer.AsString(y));
            return 0;
        }

        /// <summary>
        /// Compare two strings.
        /// </summary>
        /// <param name="first">The first string.</param>
        /// <param name="second">The second string.</param>
        /// <param name="caseInsensitive">Case invariant flag.</param>
        /// <returns>Zero if the strings are the same, negative if the first is less, positive
        /// if the second is less.</returns>
        private static int Compare(char[] first, char[] second, bool caseInsensitive)
        {
            int diff = first.Length - second.Length;
            if (diff != 0)
            {
                return diff;
            }

            int len = first.Length;
            for (int i = 0; i < len; i++)
            {
                diff = caseInsensitive
                           ? Char.ToLower(first[i]) - Char.ToLower(second[i])
                           : first[i] - second[i];
                if (diff != 0)
                {
                    return diff;
                }
            }

            return 0;
        }

        /// <summary>
        /// Convert a string into a number, in a given number base.
        /// </summary>
        /// <param name="x">The value to convert.  This is first converted to a string, 
        ///     then parsed as a number.</param>
        /// <param name="y">The number base.  If not a number, then base 10 is used.</param>
        /// <returns>The number represented by the string.</returns>
        private static Obj ToNumber(object x, object y)
        {
            int numberBase = Number.Is(y) ? (int)Number.As(y) : 10;
            try
            {
                return numberBase == 10
                           ? Double.Parse(Printer.AsString(x, false))
                           : Number.As(Convert.ToInt64(Printer.AsString(x, false), numberBase));
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