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
    /// Strings are mutable, butonly through the set! and fill! primitives.
    /// </summary>
    public class SchemeString : IPrintable, ISchemeType
    {
        #region Fields
        /// <summary>
        /// The scheme string is stored as a character array so that we can
        ///   modify it.
        /// </summary>
        private readonly char[] str;
        #endregion
        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class.
        /// </summary>
        /// <param name="str">The string contents.</param>
        private SchemeString(char[] str)
        {
            this.str = str;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class.
        /// </summary>
        /// <param name="length">The string length.</param>
        private SchemeString(int length)
        {
            this.str = new char[length];
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.String); }
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the character array from the SchemeString.
        /// </summary>
        public char[] Str
        {
            get { return this.str; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public static bool Is(Obj obj)
        {
            return obj is SchemeString;
        }

        /// <summary>
        /// Check that the object is a scheme string, and
        /// convert it into a C# string.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The C# string of the schee string.</returns>
        public static string AsString(Obj obj)
        {
            if (obj.IsSchemeString())
            {
                return new string(obj.AsSchemeString().str);
            }

            ErrorHandlers.TypeError(typeof(SchemeString), obj);
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
            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.7">(list->string <chars>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("list->string"), 
                    (args, caller) => ListToString(args.First()), 
                    1, 
                    TypePrimitives.ValueType.PairOrEmpty)
                //// <r4rs section="6.7">(make-string <k>)</r4rs>
                //// <r4rs section="6.7">(make-string <k> <char>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("make-string"), 
                    (args, caller) => New(args.First(), args.Second()), 
                    1, 
                    2, 
                    TypePrimitives.ValueType.Number, 
                    TypePrimitives.ValueType.Char)
                //// <r4rs section="6.7">(string <char> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string"), 
                    (args, caller) => ListToString(args), 
                    0, 
                    MaxInt,
                    TypePrimitives.ValueType.Char)
                //// <r4rs section="6.7">(string->list <string>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string->list"), 
                    (args, caller) => ToList(args.First()), 
                    1, 
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.5.6">(string->number <number>)</r4rs>
                //// <r4rs section="6.5.6">(string->number <number> <radix>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string->number"),
                    (args, caller) => ToNumber(args.First(), args.Second()),
                    1, 
                    2, 
                    TypePrimitives.ValueType.String, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.7">(string-append <string> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-append"), 
                    (args, caller) => Append(args),
                    0, 
                    MaxInt,
                    TypePrimitives.ValueType.String)
                .DefinePrimitive(
                    Symbol.New("string-concat"), 
                    (args, caller) => Append(args.First()), 
                    1, 
                    TypePrimitives.ValueType.Pair)
                //// <r4rs section="6.7">(string-ci<=? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-ci<=?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) <= 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string-ci<? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-ci<?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) < 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string-ci=? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-ci=?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) == 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string-ci>=? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-ci>=?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) >= 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string-ci>? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-ci>?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) > 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string-copy <string>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-copy"), 
                    (args, caller) => Copy(args.First()), 
                    1, 
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string-fill! <string> <char>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-fill!"), 
                    (args, caller) => Fill(args.First(), args.Second()), 
                    2, 
                    TypePrimitives.ValueType.String, 
                    TypePrimitives.ValueType.Char)
                //// <r4rs section="6.7">(string-length <string>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-length"), 
                    (args, caller) => Length(args.First().AsSchemeString().str), 
                    1, 
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string-ref <string> <k>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-ref"),
                    (args, caller) => Character.New(args.First().AsSchemeString().str[args.Second().AsInt()]),
                    2,
                    TypePrimitives.ValueType.String,
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.7">(string-set! <string> <k> <char>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string-set!"),
                    (args, caller) => Set(args.First(), args.Second(), args.Third()),
                    3,
                    TypePrimitives.ValueType.String,
                    TypePrimitives.ValueType.Number,
                    TypePrimitives.ValueType.Char)
                //// <r4rs section="6.7">(string<=? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string<=?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) <= 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string<?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) < 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string=? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string=?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) == 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string>=? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string>=?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) >= 0),
                    2,
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string<? <string1> <string2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string>?"),
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) > 0),
                    2, 
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.7">(string? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("string?"), 
                    (args, caller) => SchemeBoolean.Truth(args.First().IsSchemeString()), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.7">(substring <string> <start> <end>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("substring"), 
                    (args, caller) => Substr(args.First(), args.Second(), args.Third()), 
                    3, 
                    TypePrimitives.ValueType.String, 
                    TypePrimitives.ValueType.Number, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.4">(symbol->string <symbol>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("symbol->string"), 
                    (args, caller) => New(args.First()), 
                    1,
                    TypePrimitives.ValueType.Symbol);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Make a scheme string of the given length
        /// </summary>
        /// <param name="length">string length</param>
        /// <returns>The scheme string.</returns>
        public static SchemeString New(int length)
        {
            return new SchemeString(length);
        }

        /// <summary>
        /// Creates a scheme string from a string builder.
        /// </summary>
        /// <param name="buf">A string builder containing the string value.</param>
        /// <returns>The new scheme string.</returns>
        public static SchemeString New(StringBuilder buf)
        {
            return new SchemeString(buf.ToString().ToCharArray());
        }

        /// <summary>
        /// Make a scheme string from the given object.
        /// The object must be a symbol
        /// </summary>
        /// <param name="str">The symbol to convert.</param>
        /// <returns>The scheme string.</returns>
        public static SchemeString New(Obj str)
        {
            return new SchemeString(str.AsSymbol().ToString().ToCharArray());
        }

        /// <summary>
        /// Make a scheme string from the given CLR string.
        /// </summary>
        /// <param name="str">The CLR string.</param>
        /// <returns>The scheme string.</returns>
        public static SchemeString New(string str)
        {
            return new SchemeString(str.ToCharArray());
        }

        /// <summary>
        /// Make a new copy of the given scheme string.
        /// </summary>
        /// <param name="str">The existing scheme string.</param>
        /// <returns>A copy of the scheme string.</returns>
        public static SchemeString New(char[] str)
        {
            return new SchemeString(str);
        }

        /// <summary>
        /// Tests two strings for equality.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme string..</param>
        /// <param name="obj2">The second object.</param>
        /// <returns>True if the strings are equal.</returns>
        public static SchemeBoolean Equal(Obj obj1, Obj obj2)
        {
            if (!obj2.IsSchemeString())
            {
                return SchemeBoolean.False;
            }

            var str1 = obj1.AsSchemeString().Str;
            var str2 = obj2.AsSchemeString().Str;
            int len1 = str1.Length;
            int len2 = str2.Length;

            if (len1 != len2)
            {
                return SchemeBoolean.False;
            }

            for (int i = 0; i < len1; i++)
            {
                if (str1[i] != str2[i])
                {
                    return SchemeBoolean.False;
                }
            }

            return SchemeBoolean.True;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the string to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            if (!quoted)
            {
                buf.Append(this.str);
                return;
            }

            buf.Append('"');

            if (this.str != null)
            {
                foreach (char c in this.str)
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
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Convert a list of chars into a string.
        /// </summary>
        /// <param name="chars">The obj that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        internal static SchemeString ListToString(Obj chars)
        {
            var str = new StringBuilder();
            while (chars.IsPair())
            {
                str.Append(chars.First().AsCharacter().C);
                chars = chars.Rest();
            }

            return New(str);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Creates a scheme string from a length and fill character.
        /// </summary>
        /// <param name="length">The length of the string to make.</param>
        /// <param name="fill">If present, the character to fill the string with.</param>
        /// <returns>The new scheme string.</returns>
        private static SchemeString New(Obj length, Obj fill)
        {
            char c = fill.IsEmptyList() ? (char)0 : fill.AsCharacter().C;
            int len = length.AsInt();
            var res = new SchemeString(len);
            return res.Fill(c);
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
        /// Return the substring of a string.
        /// </summary>
        /// <param name="str">The original string.</param>
        /// <param name="start">The starting position.</param>
        /// <param name="end">The ending position.</param>
        /// <returns>The substring starting with the starting position and ending with the ending position.</returns>
        private static SchemeString Substr(Obj str, Obj start, Obj end)
        {
            var startPos = start.AsInt();
            var endPos = end.AsInt();
            var len = endPos - startPos;
            var newStr = new char[len];
            for (int i = 0; i < len; i++)
            {
                newStr[i] = str.AsSchemeString().str[startPos + i];
            }

            return new SchemeString(newStr);
        }

        /// <summary>
        /// Convert a string to a list of characters.
        /// </summary>
        /// <param name="s">The string to convert.</param>
        /// <returns>A list of the characters.</returns>
        private static Obj ToList(Obj s)
        {
            Obj result = EmptyList.New();
            char[] str = s.AsSchemeString().str;
            for (int i = str.Length - 1; i >= 0; i--)
            {
                result = Character.New(str[i]).Cons(result);
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
        private static Obj Set(Obj str, Obj index, Obj chr)
        {
            str.AsSchemeString().str[index.AsInt()] = chr.AsCharacter().C;
            return Undefined.New();
        }

        /// <summary>
        /// Make a copy of the given string.
        /// </summary>
        /// <param name="str">The string to copy.</param>
        /// <returns>The return value is unspecified.</returns>
        private static Obj Copy(Obj str)
        {
            return New(str.AsSchemeString());
        }

        /// <summary>
        /// Update the string by filling it with the fill char.
        /// </summary>
        /// <param name="str">The string to fill.</param>
        /// <param name="fill">The fill character.</param>
        /// <returns>The return value is unspecified.</returns>
        private static Obj Fill(Obj str, Obj fill)
        {
            char[] ss = str.AsSchemeString().str;
            for (int i = 0; i < ss.Length; i++)
            {
                ss[i] = fill.AsCharacter().C;
            }

            return Undefined.New();
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        private static SchemeString Append(Obj args)
        {
            var result = new StringBuilder();

            while (args.IsPair())
            {
                result.Append(Printer.AsString(args.First(), false));
                args = args.Rest();
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
            if (x.IsSchemeString() && y.IsSchemeString())
            {
                return Compare(x.AsSchemeString(), y.AsSchemeString(), caseInsensitive);
            }

            ErrorHandlers.SemanticError(
                string.Format(
                  @"String.Compare: expected two strings, got: ""{0}"" and ""{1}""",
                  Printer.AsString(x),
                  Printer.AsString(y)));
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
        private static int Compare(SchemeString first, SchemeString second, bool caseInsensitive)
        {
            var str1 = first.Str;
            var str2 = second.Str;
            int diff = str1.Length - str2.Length;
            if (diff != 0)
            {
                return diff;
            }

            int len = str1.Length;
            for (int i = 0; i < len; i++)
            {
                diff = caseInsensitive
                           ? char.ToLower(str1[i]) - char.ToLower(str2[i])
                           : str1[i] - str2[i];
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
        /// <param name="val">The value to convert.  This is first converted to a string, 
        ///     then parsed as a number.</param>
        /// <param name="bas">The number base.  If not a number, then base 10 is used.</param>
        /// <returns>The number represented by the string.</returns>
        private static Obj ToNumber(Obj val, Obj bas)
        {
            int numberBase = bas.IsNumber() ? bas.AsInt() : 10;
            try
            {
                if (numberBase == 10)
                {
                    return Number.New(double.Parse(Printer.AsString(val, false)));
                }

                return Number.New(Convert.ToInt64(Printer.AsString(val, false), numberBase));
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

        #region Private Methods
        /// <summary>
        /// Fill the string up with the given char.
        /// </summary>
        /// <param name="fill">The filler character.</param>
        /// <returns>A new string make up of the fill character.</returns>
        private SchemeString Fill(char fill)
        {
            for (int i = 0; i < this.str.Length; i++)
            {
                this.str[i] = fill;
            }

            return this;
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extension class for SchemeString
    /// </summary>
    public static class SchemeStringExtension
    {
        /// <summary>
        /// Tests whether to given object is a scheme string.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme string.</returns>
        public static bool IsSchemeString(this Obj obj)
        {
            return SchemeString.Is(obj);
        }

        /// <summary>
        /// Convert to scheme string
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding scheme string.</returns>
        public static SchemeString AsSchemeString(this Obj x)
        {
            if (SchemeString.Is(x))
            {
                return (SchemeString)x;
            }

            ErrorHandlers.TypeError(typeof(SchemeString), x);
            return null;
        }

        /// <summary>
        /// Convert to string
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding string.</returns>
        public static string AsString(this Obj x)
        {
            if (SchemeString.Is(x))
            {
                return SchemeString.AsString(x);
            }

            ErrorHandlers.TypeError(typeof(SchemeString), x);
            return null;
        }
    } 
    #endregion
}