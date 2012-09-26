// <copyright file="SchemeString.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// Handles scheme strings.
    /// Strings are represented as a character array.
    /// Strings are mutable, butonly through the set! and fill! primitives.
    /// </summary>
    public class SchemeString : SchemeObject
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

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class from a string builder. 
        /// </summary>
        /// <param name="buf">A string builder containing the string value.</param>
        private SchemeString(StringBuilder buf) : this(buf.ToString().ToCharArray())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class from a symbol. 
        /// Make a scheme string from the given object.
        /// </summary>
        /// <param name="str">The symbol to convert.</param>
        /// <returns>The scheme string.</returns>
        private SchemeString(Symbol str) : this(str.ToString().ToCharArray())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class from a SchemeString. 
        /// Make a scheme string from the given object.
        /// </summary>
        /// <param name="str">The scheme string to copy.</param>
        /// <returns>The scheme string.</returns>
        private SchemeString(SchemeString str) : this(str.ToString().ToCharArray())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class from the given CLR string. 
        /// </summary>
        /// <param name="str">The CLR string.</param>
        /// <returns>The scheme string.</returns>
        private SchemeString(string str) : this(str.ToCharArray())
        {
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

        #region New
        /// <summary>
        /// Converts a char[] into a SchemeString.
        /// </summary>
        /// <param name="str">The char[].</param>
        /// <returns>The corresponding SchemeString.</returns>
        public static implicit operator SchemeString(char[] str)
        {
            return New(str);
        }

        /// <summary>
        /// Converts a StringBuilder into a SchemeString.
        /// </summary>
        /// <param name="buf">The StringBuilder.</param>
        /// <returns>The corresponding SchemeString.</returns>
        public static implicit operator SchemeString(StringBuilder buf)
        {
            return New(buf);
        }

        /// <summary>
        /// Converts a string into a SchemeString.
        /// </summary>
        /// <param name="str">The string.</param>
        /// <returns>The corresponding SchemeString.</returns>
        public static implicit operator SchemeString(string str)
        {
            return New(str);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class.
        /// </summary>
        /// <param name="str">The string contents.</param>
        /// <returns>A SchemeString.</returns>
        public static SchemeString New(char[] str)
        {
            return new SchemeString(str);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeString"/> class.
        /// </summary>
        /// <param name="length">The string length.</param>
        /// <returns>A SchemeString.</returns>
        public static SchemeString New(int length)
        {
            return new SchemeString(length);
        }

        /// <summary>
        /// Creates a scheme string from a string builder.
        /// </summary>
        /// <param name="buf">A string builder containing the string value.</param>
        /// <returns>A SchemeString.</returns>
        public static SchemeString New(StringBuilder buf)
        {
            return new SchemeString(buf);
        }

        /// <summary>
        /// Make a scheme string from the given CLR string.
        /// </summary>
        /// <param name="str">The CLR string.</param>
        /// <returns>The scheme string.</returns>
        /// <returns>A SchemeString.</returns>
        public static SchemeString New(string str)
        {
            return new SchemeString(str);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Check that the object is a scheme string, and
        /// convert it into a C# string.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The C# string of the schee string.</returns>
        public static string AsString(SchemeObject obj)
        {
            if (obj is SchemeString)
            {
                return new string(((SchemeString)obj).str);
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
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            const int MaxInt = int.MaxValue;
            env
                .DefinePrimitive(
                    "list->string", 
                    new[] { "6.7", "(list->string <chars>)" },
                    (args, caller) => ListToString(First(args)), 
                    1, 
                    Primitive.ArgType.PairOrEmpty)
                .DefinePrimitive(
                    "make-string", 
                    new[] { "6.7", "(make-string <k>)", "(make-string <k> <char>)" },
                    (args, caller) => New((Number)First(args), Second(args)), 
                    1, 
                    2, 
                    Primitive.ArgType.Number, 
                    Primitive.ArgType.CharOrEmpty)
                .DefinePrimitive(
                    "string", 
                    new[] { "6.7", "(string <char> ...)" },
                    (args, caller) => ListToString(args), 
                    0, 
                    MaxInt,
                    Primitive.ArgType.Char)
                .DefinePrimitive(
                    "string->list", 
                    new[] { "6.7", "(string->list <string>)" },
                    (args, caller) => ToList((SchemeString)First(args)), 
                    1, 
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string->number", 
                    new[] { "6.5.6", "(string->number <number>)", "(string->number <number> <radix>)" },
                    (args, caller) => ToNumber(First(args), Second(args)),
                    1, 
                    2, 
                    Primitive.ArgType.String, 
                    Primitive.ArgType.Number)
                .DefinePrimitive(
                    "string-append", 
                    new[] { "6.7", "(string-append <string> ...)" },
                    (args, caller) => Append(args),
                    0, 
                    MaxInt,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-concat", 
                    new[] { "(string-concat <string> ...)" },
                    (args, caller) => Append(First(args)), 
                    1, 
                    Primitive.ArgType.Pair)
                .DefinePrimitive(
                    "string-ci<=?", new[] { "6.7", "(string-ci<=? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), true) <= 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-ci<?", 
                    new[] { "6.7", "(string-ci<? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), true) < 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-ci=?", 
                    new[] { "6.7", "(string-ci=? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), true) == 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-ci>=?", 
                    new[] { "6.7", "(string-ci>=? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), true) >= 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-ci>?", 
                    new[] { "6.7", "(string-ci>? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), true) > 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-copy", new[] { "6.7", "(string-copy <string>)" }, 
                    (args, caller) => Copy((SchemeString)First(args)), 
                    1, 
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-fill!", 
                    new[] { "6.7", "(string-fill! <string> <char>)" }, 
                    (args, caller) => Fill((SchemeString)First(args), (Character)Second(args)), 
                    2, 
                    Primitive.ArgType.String, 
                    Primitive.ArgType.Char)
                .DefinePrimitive(
                    "string-length", 
                    new[] { "6.7", "(string-length <string>)" }, 
                    (args, caller) => (Number)Length(((SchemeString)First(args)).str),
                    1, 
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string-ref", 
                    new[] { "6.7", "(string-ref <string> <k>)" },
                    (args, caller) => (Character)((SchemeString)First(args)).str[Number.AsInt(Second(args))],
                    2,
                    Primitive.ArgType.String,
                    Primitive.ArgType.Number)
                .DefinePrimitive(
                    "string-set!", 
                    new[] { "6.7", "(string-set! <string> <k> <char>)" },
                    (args, caller) => Set((SchemeString)First(args), (Number)Second(args), (Character)Third(args)),
                    3,
                    Primitive.ArgType.String,
                    Primitive.ArgType.Number,
                    Primitive.ArgType.Char)
                .DefinePrimitive(
                    "string<=?", 
                    new[] { "6.7", "(string<=? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), false) <= 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string<?", 
                    new[] { "6.7", "(string<? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), false) < 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string=?", 
                    new[] { "6.7", "(string=? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), false) == 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string>=?", 
                    new[] { "6.7", "(string>=? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), false) >= 0),
                    2,
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string>?", 
                    new[] { "6.7", "(string<? <string1> <string2>)" },
                    (args, caller) => SchemeBoolean.Truth(Compare((SchemeString)First(args), (SchemeString)Second(args), false) > 0),
                    2, 
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    "string?", 
                    new[] { "6.7", "(string? <obj>)" },
                    (args, caller) => SchemeBoolean.Truth(First(args) is SchemeString), 
                    1, 
                    Primitive.ArgType.Obj)
                .DefinePrimitive(
                    "substring", 
                    new[] { "6.7", "(substring <string> <start> <end>)" }, 
                    (args, caller) => Substr((SchemeString)First(args), Second(args), Third(args)), 
                    3, 
                    Primitive.ArgType.String, 
                    Primitive.ArgType.Number, 
                    Primitive.ArgType.Number)
                .DefinePrimitive(
                    "symbol->string", 
                    new[] { "6.4", "(symbol->string <symbol>)" }, 
                    (args, caller) => new SchemeString((Symbol)First(args)), 
                    1,
                    Primitive.ArgType.Symbol);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests two strings for equality.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme string..</param>
        /// <param name="obj2">The second object.</param>
        /// <returns>True if the strings are equal.</returns>
        public static SchemeBoolean Equal(SchemeString obj1, SchemeObject obj2)
        {
            if (!(obj2 is SchemeString))
            {
                return false;
            }

            var str1 = obj1.Str;
            var str2 = ((SchemeString)obj2).Str;
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

        #region Equality
        /// <summary>
        /// Provide our own version of the Equals method.
        /// </summary>
        /// <param name="other">The other object.</param>
        /// <returns>True if they are equal character arrays.</returns>
        public override bool Equals(object other)
        {
            if (!(other is SchemeString))
            {
                return false;
            }

            return this.Equals((SchemeString)other);
        }

        /// <summary>
        /// Compares two Number values by comparing their underlying character arrays.
        /// </summary>
        /// <param name="other">The other SchemeString.</param>
        /// <returns>True if they have the same characters.</returns>
        public bool Equals(SchemeString other)
        {
            for (int i = 0; i < this.str.Length; i++)
            {
                if (this.str[i] != other.str[i])
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// The hash code is the SchemeString's hash code.
        /// </summary>
        /// <returns>The hash code.</returns>
        public override int GetHashCode()
        {
            int hash = 0;
            for (int i = 0; i < this.str.Length; i++)
            {
                hash += this.str[i];
            }

            return hash;
        }
        #endregion

        #region Public Methods

        /// <summary>
        /// Gets the string representation of the SchemeString.
        /// </summary>
        /// <returns>The string.</returns>
        public override string ToString()
        {
            return new string(this.str);
        }

        /// <summary>
        /// Write the string to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
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

        /// <summary>
        /// Describe a scheme string by returning its value.
        /// </summary>
        /// <returns>The scheme string as a string.</returns>
        public override string Describe()
        {
            return this.ToString();
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Convert a list of chars into a string.
        /// </summary>
        /// <param name="chars">The obj that is a list of chars.</param>
        /// <returns>The caracter array made up of the chars.</returns>
        internal static SchemeString ListToString(SchemeObject chars)
        {
            var str = new StringBuilder();
            while (chars is Pair)
            {
                var ch = First(chars);
                if (!(ch is Character))
                {
                    ErrorHandlers.TypeError(typeof(Character), ch);
                    return null;
                }

                str.Append(((Character)ch).C);
                chars = Rest(chars);
            }

            return new SchemeString(str);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Creates a scheme string from a length and fill character.
        /// </summary>
        /// <param name="length">The length of the string to make.</param>
        /// <param name="fill">If present, the character to fill the string with.</param>
        /// <returns>The new scheme string.</returns>
        private static SchemeString New(Number length, SchemeObject fill)
        {
            char c = fill is EmptyList ? (char)0 : ((Character)fill).C;
            int len = Number.AsInt(length);
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
        private static SchemeString Substr(SchemeString str, SchemeObject start, SchemeObject end)
        {
            var startPos = Number.AsInt(start);
            var endPos = Number.AsInt(end);
            var len = endPos - startPos;
            var newStr = new char[len];
            Array.Copy(str.str, startPos, newStr, 0, len);

            return new SchemeString(newStr);
        }

        /// <summary>
        /// Convert a string to a list of characters.
        /// </summary>
        /// <param name="s">The string to convert.</param>
        /// <returns>A list of the characters.</returns>
        private static SchemeObject ToList(SchemeString s)
        {
            SchemeObject result = EmptyList.Instance;
            char[] str = s.str;
            for (int i = str.Length - 1; i >= 0; i--)
            {
                result = Cons((Character)str[i], result);
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
        private static SchemeObject Set(SchemeString str, Number index, Character chr)
        {
            str.str[Number.AsInt(index)] = chr.C;
            return Undefined.Instance;
        }

        /// <summary>
        /// Make a copy of the given string.
        /// </summary>
        /// <param name="str">The string to copy.</param>
        /// <returns>The return value is unspecified.</returns>
        private static SchemeObject Copy(SchemeString str)
        {
            return new SchemeString(str);
        }

        /// <summary>
        /// Update the string by filling it with the fill char.
        /// </summary>
        /// <param name="str">The string to fill.</param>
        /// <param name="fill">The fill character.</param>
        /// <returns>The return value is unspecified.</returns>
        private static SchemeObject Fill(SchemeString str, Character fill)
        {
            str.Fill(fill.C);

            return Undefined.Instance;
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        private static SchemeString Append(SchemeObject args)
        {
            var result = new StringBuilder();

            while (args is Pair)
            {
                result.Append(First(args).ToString(false));
                args = Rest(args);
            }

            return new SchemeString(result);
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
        private static SchemeObject ToNumber(SchemeObject val, SchemeObject bas)
        {
            int numberBase = bas is Number ? Number.AsInt(bas) : 10;
            try
            {
                if (numberBase == 10)
                {
                    return (Number)double.Parse(val.ToString(false));
                }

                return (Number)Convert.ToInt64(val.ToString(false), numberBase);
            }
            catch (FormatException)
            {
                return (SchemeBoolean)false;
            }
            catch (ArgumentException)
            {
                return (SchemeBoolean)false;
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
        /// Convert to string
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding string.</returns>
        public static string AsString(this SchemeObject x)
        {
            if (x is SchemeString)
            {
                return SchemeString.AsString(x);
            }

            ErrorHandlers.TypeError(typeof(SchemeString), x);
            return null;
        }
    } 
    #endregion
}