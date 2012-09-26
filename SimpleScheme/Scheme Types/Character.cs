// <copyright file="Character.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;

    /// <summary>
    /// Handles a scheme character.
    /// Scheme characters are represented as .NET char objects.
    /// </summary>
    public class Character : SchemeObject, IEquatable<Character>
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
        /// Initializes static members of the <see cref="Character"/> class. 
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

        /// <summary>
        /// Initializes a new instance of the <see cref="Character"/> class.
        /// </summary>
        /// <param name="c">The character.</param>
        private Character(char c)
        {
            this.c = c;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Character"/> class.
        /// </summary>
        /// <param name="c">The character.</param>
        /// <param name="lineNumber">The line where the character is read.</param>
        private Character(char c, int lineNumber) : base(lineNumber)
        {
            this.c = c;
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

        /// <summary>
        /// Create a new Character.
        /// </summary>
        /// <param name="c">The char.</param>
        /// <param name="lineNumber">The line where the character is read.</param>
        /// <returns>The corresponding Character.</returns>
        internal static Character New(char c, int lineNumber)
        {
            Character res;
            if (fixedChars.TryGetValue(c, out res))
            {
                return res;
            }

            return new Character(c, lineNumber);
        }
        #endregion

        #region Private Static
        /// <summary>
        /// Tests whether two Characters are equal.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme Character).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both the same character.</returns>
        internal static SchemeBoolean Equal(Character obj1, SchemeObject obj2)
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
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            primEnv
                .DefinePrimitive(
                        "char->integer", 
                        new[] { "6.6", "(char->integer <char>)" },
                        (args, env, caller) => (Number)((Character)First(args)).C,
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                        "char-alphabetic?", 
                        new[] { "6.6", "(char-alphabetic? <char>)" },
                        (args, env, caller) => SchemeBoolean.Truth(char.IsLetter(((Character)First(args)).C)), 
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                        "char-ci<=?", 
                        new[] { "6.6", "(char-ci<=? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) <= 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char-ci<?", 
                        new[] { "6.6", "(char-ci<? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) < 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char-ci=?", 
                        new[] { "6.6", "(char-ci=? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) == 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char-ci>=?", 
                        new[] { "6.6", "(char-ci>=? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) >= 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char-ci>?", 
                        new[] { "6.6", "(char-ci>? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), true) > 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char-downcase", 
                        new[] { "(char-downcase <char>)" },
                        (args, env, caller) => (Character)char.ToLower(((Character)First(args)).C), 
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                        "char-lower-case?", 
                        new[] { "6.6", "(char-lower-case? <letter>)" },
                        (args, env, caller) => SchemeBoolean.Truth(char.IsLower(((Character)First(args)).C)), 
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                        "char-numeric?", 
                        new[] { "6.6", "(char-numeric? <char>)" },
                        (args, env, caller) => SchemeBoolean.Truth(char.IsDigit(((Character)First(args)).C)), 
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                        "char-upcase", 
                        new[] { "6.6", "(char-upcase <char>)" },
                        (args, env, caller) => (Character)char.ToUpper(((Character)First(args)).C), 
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                        "char-upper-case?", 
                        new[] { "6.6", "(char-upper-case? <letter>)" },
                        (args, env, caller) => SchemeBoolean.Truth(char.IsUpper(((Character)First(args)).C)), 
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                        "char-whitespace?", 
                        new[] { "6.6", "(char-chitespace? <char>)" },
                        (args, env, caller) => SchemeBoolean.Truth(char.IsWhiteSpace(((Character)First(args)).C)), 
                        new ArgsInfo(1, ArgType.Char))
                .DefinePrimitive(
                       "char<=?", 
                       new[] { "6.6", "(char<=? <char1> <char2>)" },
                       (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) <= 0), 
                       new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char<?", 
                        new[] { "6.6", "(char<? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) < 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char=?", 
                        new[] { "6.6", "(char=? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) == 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char>=?", 
                        new[] { "6.6", "(char>=? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) >= 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char>?", 
                        new[] { "6.6", "(char>? <char1> <char2>)" },
                        (args, env, caller) => SchemeBoolean.Truth(Compare((Character)First(args), (Character)Second(args), false) > 0), 
                        new ArgsInfo(2, ArgType.Char))
                .DefinePrimitive(
                        "char?", 
                        new[] { "6.6", "(char? <obj>)" },
                        (args, env, caller) => SchemeBoolean.Truth(First(args) is Character), 
                        new ArgsInfo(1, ArgType.Obj));
        }
        #endregion

        #region Equality
        /// <summary>
        /// Provide our own version of the Equals method.
        /// </summary>
        /// <param name="other">The other object.</param>
        /// <returns>True if they are equal as characters.</returns>
        public override bool Equals(object other)
        {
            if (!(other is Character))
            {
                return false;
            }

            return this.Equals((Character)other);
        }

        /// <summary>
        /// Compares two Character values by comparing their underlying character.
        /// </summary>
        /// <param name="other">The other Character.</param>
        /// <returns>True if they have the same char.</returns>
        public bool Equals(Character other)
        {
            return this.c == other.c;
        }

        /// <summary>
        /// The hash code is simply the character.
        /// </summary>
        /// <returns>The hash code.</returns>
        public override int GetHashCode()
        {
            return this.c;
        }
        #endregion

        #region CLR Type Converters
        /// <summary>
        /// Conver to a char.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding char.</returns>
        internal static char AsChar(SchemeObject x)
        {
            if (x is Character)
            {
                return ((Character)x).C;
            }

            ErrorHandlers.TypeError(typeof(Character), x);
            return '\0';
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Return the character, possibly quoted, as a string.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <returns>The character as a string.</returns>
        internal override string ToString(bool quoted)
        {
            string prefix = quoted ? "#\\" : string.Empty;
            if (this.c == ' ')
            {
                return prefix + "space";
            }

            return prefix + this.c;
        }

        /// <summary>
        /// Describe a character by returning its value.
        /// </summary>
        /// <returns>The character as a string.</returns>
        internal override string Describe()
        {
            return this.ToString();
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