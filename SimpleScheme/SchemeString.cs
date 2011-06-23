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
    public sealed class SchemeString : ListPrimitives, IEnumerable<char>
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
        public int StringLength
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
        /// Define the string primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.6">(char->integer <char>)</r4rs>
                .DefinePrimitive("char->integer", (caller, args) => (double)Chr(First(args)), 1)
                //// <r4rs section="6.6">(char-alphabetic? <char>)</r4rs>
                .DefinePrimitive("char-alphabetic?", (caller, args) => SchemeBoolean.Truth(char.IsLetter(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-ci<=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci<=?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) <= 0), 2)
                //// <r4rs section="6.6">(char-ci<? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci<?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) < 0), 2)
                //// <r4rs section="6.6">(char-ci=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci=?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) == 0), 2)
                //// <r4rs section="6.6">(char-ci>=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci>=?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) >= 0), 2)
                //// <r4rs section="6.6">(char-ci>? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci>?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) > 0), 2)
                //// <r4rs section="6.6">(char-downcase <char>)</r4rs>
                .DefinePrimitive("char-downcase", (caller, args) => Chr(char.ToLower(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-lower-case? <letter>)</r4rs>
                .DefinePrimitive("char-lower-case?", (caller, args) => SchemeBoolean.Truth(char.IsLower(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-numeric? <char>)</r4rs>
                .DefinePrimitive("char-numeric?", (caller, args) => SchemeBoolean.Truth(char.IsDigit(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-upcase <char>)</r4rs>
                .DefinePrimitive("char-upcase", (caller, args) => Chr(char.ToUpper(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-upper-case? <letter>)</r4rs>
                .DefinePrimitive("char-upper-case?", (caller, args) => SchemeBoolean.Truth(char.IsUpper(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-chitespace? <char>)</r4rs>
                .DefinePrimitive("char-whitespace?", (caller, args) => SchemeBoolean.Truth(char.IsWhiteSpace(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char<=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char<=?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) <= 0), 2)
                //// <r4rs section="6.6">(char<? <char1> <char2>)</r4rs>
                .DefinePrimitive("char<?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) < 0), 2)
                //// <r4rs section="6.6">(char=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char=?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) == 0), 2)
                //// <r4rs section="6.6">(char>=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char>=?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) >= 0), 2)
                //// <r4rs section="6.6">(char>? <char1> <char2>)</r4rs>
                .DefinePrimitive("char>?", (caller, args) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) > 0), 2)
                //// <r4rs section="6.6">(char? <obj>)</r4rs>
                .DefinePrimitive("char?", (caller, args) => SchemeBoolean.Truth(First(args) is char), 1)

                //// <r4rs section="6.7">(make-string <k>)</r4rs>
                //// <r4rs section="6.7">(make-string <k> <char>)</r4rs>
                .DefinePrimitive("make-string", (caller, args) => new SchemeString(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.7">(string <char> ...)</r4rs>
                .DefinePrimitive("string", (caller, args) => ListToString(args), 0, MaxInt)
                //// <r4rs section="6.7">(string->list <string>)</r4rs>
                .DefinePrimitive("string->list", (caller, args) => StringToList(First(args)), 1)
                //// <r4rs section="6.5.6">(string->number <number>)</r4rs>
                //// <r4rs section="6.5.6">(string->number <number> <radix>)</r4rs>
                .DefinePrimitive("string->number", (caller, args) => StringToNumber(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.4">(string->symbol <string>)</r4rs>
                .DefinePrimitive("string->symbol", (caller, args) => string.Intern(Str(First(args)).AsString()), 1)
                //// <r4rs section="6.7">(string-append <string> ...)</r4rs>
                .DefinePrimitive("string-append", (caller, args) => StringAppend(args), 0, MaxInt)
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
                .DefinePrimitive("string-length", (caller, args) => Number.Num(Str(First(args)).StringLength), 1)
                //// <r4rs section="6.7">(string-ref <string> <k>)</r4rs>
                .DefinePrimitive("string-ref", (caller, args) => Chr(Str(First(args))[(int)Number.Num(Second(args))]), 2)
                //// <r4rs section="6.7">(string-set! <string> <k> <char>)</r4rs>
                .DefinePrimitive(
                   "string-set!",
                   (caller, args) =>
                   {
                       object z = Third(args);
                       Str(First(args))[(int)Number.Num(Second(args))] = Chr(z);
                       return z;
                   },
                    3)
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
                .DefinePrimitive("string?", (caller, args) => SchemeBoolean.Truth(First(args) is SchemeString), 1)
                //// <r4rs section="6.7">(substring <string> <start> <end>)</r4rs>
                .DefinePrimitive(
                    "substring",
                    (caller, args) =>
                    {
                        int start = (int)Number.Num(Second(args));
                        int end = (int)Number.Num(Third(args));
                        return Str(First(args)).Substring(start, end - start);
                    },
                    3)
                //// <r4rs section="6.4">(symbol->string <symbol>)</r4rs>
                .DefinePrimitive("symbol->string", (caller, args) => new SchemeString(Sym(First(args))), 1)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive("symbol?", (caller, args) => SchemeBoolean.Truth(First(args) is string), 1);
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
            else if (x is SchemeString)
            {
                ((SchemeString)x).AsString(quoted, buf);
            }
            else if (x is object[])
            {
                Vector.AsString((object[])x, quoted, buf);
            }
            else if (x is Stepper)
            {
                buf.Append(((Stepper)x).Expr);
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
        /// Covert the SchemeString into a string.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public void AsString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append('"');
            }

            foreach (char c in this)
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

        /// <summary>
        /// Gets the SchemeString as a string.
        /// </summary>
        /// <returns>The contained string.</returns>
        public string AsString()
        {
            return this.str;
        }

        /// <summary>
        /// Format as a string.
        /// </summary>
        /// <returns>The SchemeString as a string.</returns>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            this.AsString(false, sb);
            return sb.ToString();
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
        /// Compare two strings.
        /// </summary>
        /// <param name="xstr">The first string.</param>
        /// <param name="ystr">The second string.</param>
        /// <param name="ci">Case invariant flag.</param>
        /// <returns>Zero if the strings are the same, negative if the first is less, positive
        /// if the second is less.</returns>
        private static int Compare(SchemeString xstr, SchemeString ystr, bool ci)
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
        /// Turn an object (storing a string) into an array of characters.
        /// </summary>
        /// <param name="x">The string object.</param>
        /// <returns>The character array.</returns>
        private static SchemeString Str(object x)
        {
            if (x is SchemeString)
            {
                return (SchemeString)x;
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
            Pair result = null;
            SchemeString str = Str(s);
            for (int i = str.StringLength - 1; i >= 0; i--)
            {
                result = Cons(Chr(str[i]), result);
            }

            return result;
        }

        /// <summary>
        /// Make a copy of the given string.
        /// </summary>
        /// <param name="str">The string to copy.</param>
        /// <returns>The return value is unspecified.</returns>
        private static object StringCopy(object str)
        {
            return new SchemeString(Str(str).AsString());
        }

        /// <summary>
        /// Update the string by filling it with the fill char.
        /// </summary>
        /// <param name="str">The string to fill.</param>
        /// <param name="fill">The fill character.</param>
        /// <returns>The return value is unspecified.</returns>
        private static object StringFill(object str, object fill)
        {
            SchemeString ss = Str(str);
            ss.str = new string(Chr(fill), ss.StringLength);
            return null;
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        private static SchemeString StringAppend(object args)
        {
            StringBuilder result = new StringBuilder();

            if (args is Pair)
            {
                foreach (var elem in (Pair)args)
                {
                    result.Append(AsString(elem, false));
                }
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
        private static int StringCompare(object x, object y, bool ci)
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

        /// <summary>
        /// Compares two characters.
        /// </summary>
        /// <param name="x">The first char.</param>
        /// <param name="y">The second char.</param>
        /// <param name="ci">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if x is before y, positive if x is after y, 
        /// or 0 if they are the same.</returns>
        private static int ChrCompare(object x, object y, bool ci)
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
        private static string Sym(object x)
        {
            if (x is string)
            {
                return (string)x;
            }

            return Sym(ErrorHandlers.Error("Expected a symbol, got: " + x));
        }

        /// <summary>
        /// Get a substring from the string.
        /// </summary>
        /// <param name="start">The starting character position.</param>
        /// <param name="len">The number of characters.</param>
        /// <returns>A substring, starting at the given position.</returns>
        private SchemeString Substring(int start, int len)
        {
            return new SchemeString(this.str.Substring(start, len));
        }
    }
}