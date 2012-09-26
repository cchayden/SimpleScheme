// <copyright file="Character.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Text;

    /// <summary>
    /// Handles a scheme character.
    /// Scheme characters are represented as .NET char objects.
    /// </summary>
    public class Character : SchemeObject
    {
        #region Fields
        /// <summary>
        /// Static characters for most often used chars.
        /// </summary>
        private static readonly Dictionary<char, Character> fixedChars;

        /// <summary>
        /// The character.
        /// </summary>
        private readonly char c;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="Character"/> class.
        /// </summary>
        /// <param name="c">The character.</param>
        private Character(char c)
        {
            this.c = c;
        }

        /// <summary>
        /// Create some static Character objects.
        /// </summary>
        static Character()
        {
            fixedChars = new Dictionary<char, Character>();
            for (int i = 0; i < 127; i++)
            {
                fixedChars.Add((char)i, new Character((char)i));
            }
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public override string TypeName
        {
            get { return ValueTypeName(ValueType.Char); }
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the character itself.
        /// </summary>
        public char C
        {
            get { return this.c; }
        }
        #endregion

        #region New
        /// <summary>
        /// Converts a char into a Character.
        /// </summary>
        /// <param name="c">The char.</param>
        /// <returns>The corresponding Character.</returns>
        public static implicit operator Character(char c)
        {
            return New(c);
        }

        /// <summary>
        /// Create a new Character.
        /// </summary>
        /// <param name="c">The char.</param>
        /// <returns>The corresponding Character.</returns>
        public static Character New(char c)
        {
            Character res;
            if (fixedChars.TryGetValue(c, out res))
            {
                return res;
            }

            return new Character(c);
        }
        #endregion

        #region Private Static
        /// <summary>
        /// Tests whether two Characters are equal.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme Character).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both the same character.</returns>
        public static SchemeBoolean Equal(Character obj1, SchemeObject obj2)
        {
            if (!(obj2 is Character))
            {
                return false;
            }

            var char1 = obj1;
            var char2 = (Character)obj2;
            if (char1.c != char2.C)
            {
                return false;
            }

            return true;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the character primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// <r4rs section="6.6">(char->integer <char>)</r4rs>
                .DefinePrimitive(
                        "char->integer",
                        (args, caller) => (Number)((Character)First(args)).C,
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-alphabetic? <char>)</r4rs>
                .DefinePrimitive(
                        "char-alphabetic?",
                        (args, caller) => SchemeBoolean.Truth(char.IsLetter(((Character)First(args)).C)), 
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-ci<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                    "char-ci<=?",
                    (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) <= 0), 
                    2, 
                    ValueType.Char)
                //// <r4rs section="6.6">(char-ci<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci<?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) < 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-ci=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci=?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) == 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-ci>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci>=?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) >= 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-ci>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci>?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) > 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-downcase <char>)</r4rs>
                .DefinePrimitive(
                        "char-downcase",
                        (args, caller) => (Character)char.ToLower(((Character)First(args)).C), 
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-lower-case? <letter>)</r4rs>
                .DefinePrimitive(
                        "char-lower-case?",
                        (args, caller) => SchemeBoolean.Truth(char.IsLower(((Character)First(args)).C)), 
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-numeric? <char>)</r4rs>
                .DefinePrimitive(
                        "char-numeric?",
                        (args, caller) => SchemeBoolean.Truth(char.IsDigit(((Character)First(args)).C)), 
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-upcase <char>)</r4rs>
                .DefinePrimitive(
                        "char-upcase",
                        (args, caller) => (Character)char.ToUpper(((Character)First(args)).C), 
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-upper-case? <letter>)</r4rs>
                .DefinePrimitive(
                        "char-upper-case?",
                        (args, caller) => SchemeBoolean.Truth(char.IsUpper(((Character)First(args)).C)), 
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char-chitespace? <char>)</r4rs>
                .DefinePrimitive(
                        "char-whitespace?",
                        (args, caller) => SchemeBoolean.Truth(char.IsWhiteSpace(((Character)First(args)).C)), 
                        1, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                       "char<=?",
                       (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) <= 0), 
                       2, 
                       ValueType.Char)
                //// <r4rs section="6.6">(char<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char<?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) < 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char=?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) == 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char>=?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) >= 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char>?",
                        (args, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) > 0), 
                        2, 
                        ValueType.Char)
                //// <r4rs section="6.6">(char? <obj>)</r4rs>
                .DefinePrimitive(
                        "char?",
                        (args, caller) => SchemeBoolean.Truth(First(args) is Character), 
                        1, 
                        ValueType.Obj);
        }
        #endregion

        /// <summary>
        /// Conver to a char.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding char.</returns>
        public static char AsChar(SchemeObject x)
        {
            if (x is Character)
            {
                return ((Character)x).C;
            }

            ErrorHandlers.TypeError(typeof(Character), x);
            return '\0';
        }

        #region Public Methods
        /// <summary>
        /// Write the character to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("#\\");
            }

            if (this.c == ' ')
            {
                buf.Append("space");
            }
            else
            {
                buf.Append(this.c);
            }
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Compares two characters.
        /// </summary>
        /// <param name="char1">The first char.</param>
        /// <param name="char2">The second char.</param>
        /// <param name="caseInsensitive">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if x is before y, positive if x is after y, 
        /// or 0 if they are the same.</returns>
        private static int Compare(Character char1, Character char2, bool caseInsensitive)
        {
            char c1 = char1.C;
            char c2 = char2.C;
            return caseInsensitive ? char.ToLower(c1) - char.ToLower(c2) : c1 - c2;
        }
        #endregion
    }
}