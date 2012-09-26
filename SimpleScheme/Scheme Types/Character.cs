// <copyright file="Character.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Handles a scheme character.
    /// Scheme characters are represented as .NET char objects.
    /// </summary>
    public class Character : IPrintable, ISchemeObject
    {
        #region Fields
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
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Char); }
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
            // TODO cch define some standard characters that don't need to be created.
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
        public static SchemeBoolean Equal(ISchemeObject obj1, ISchemeObject obj2)
        {
            if (!(obj2 is Character))
            {
                return false;
            }

            var char1 = obj1.AsCharacter();
            var char2 = obj2.AsCharacter();
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
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// <r4rs section="6.6">(char->integer <char>)</r4rs>
                .DefinePrimitive(
                        "char->integer",
                        (args, caller) => (Number)List.First(args).AsCharacter().C,
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-alphabetic? <char>)</r4rs>
                .DefinePrimitive(
                        "char-alphabetic?",
                        (args, caller) => SchemeBoolean.Truth(char.IsLetter(List.First(args).AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                    "char-ci<=?",
                    (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) <= 0), 
                    2, 
                    TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci<?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) < 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci=?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) == 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci>=?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) >= 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char-ci>?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) > 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-downcase <char>)</r4rs>
                .DefinePrimitive(
                        "char-downcase",
                        (args, caller) => (Character)char.ToLower(List.First(args).AsCharacter().C), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-lower-case? <letter>)</r4rs>
                .DefinePrimitive(
                        "char-lower-case?",
                        (args, caller) => SchemeBoolean.Truth(char.IsLower(List.First(args).AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-numeric? <char>)</r4rs>
                .DefinePrimitive(
                        "char-numeric?",
                        (args, caller) => SchemeBoolean.Truth(char.IsDigit(List.First(args).AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-upcase <char>)</r4rs>
                .DefinePrimitive(
                        "char-upcase",
                        (args, caller) => (Character)char.ToUpper(List.First(args).AsCharacter().C), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-upper-case? <letter>)</r4rs>
                .DefinePrimitive(
                        "char-upper-case?",
                        (args, caller) => SchemeBoolean.Truth(char.IsUpper(List.First(args).AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-chitespace? <char>)</r4rs>
                .DefinePrimitive(
                        "char-whitespace?",
                        (args, caller) => SchemeBoolean.Truth(char.IsWhiteSpace(List.First(args).AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                       "char<=?",
                       (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) <= 0), 
                       2, 
                       TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char<?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) < 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char=?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) == 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char>=?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) >= 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        "char>?",
                        (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) > 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char? <obj>)</r4rs>
                .DefinePrimitive(
                        "char?",
                        (args, caller) => SchemeBoolean.Truth(List.First(args) is Character), 
                        1, 
                        TypePrimitives.ValueType.Obj);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the character to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
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
        private static int Compare(ISchemeObject char1, ISchemeObject char2, bool caseInsensitive)
        {
            char c1 = char1.AsCharacter().C;
            char c2 = char2.AsCharacter().C;
            return caseInsensitive ? char.ToLower(c1) - char.ToLower(c2) : c1 - c2;
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extension class for Character
    /// </summary>
    public static class CharacterExtension
    {
        /// <summary>
        /// Convert to a character.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding character.</returns>
        public static Character AsCharacter(this ISchemeObject x)
        {
            if (x is Character)
            {
                return (Character)x;
            }

            ErrorHandlers.TypeError(typeof(Character), x);
            return null;
        }

        /// <summary>
        /// Conver to a char.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding char.</returns>
        public static char AsChar(this ISchemeObject x)
        {
            if (x is Character)
            {
                return ((Character)x).C;
            }

            ErrorHandlers.TypeError(typeof(Character), x);
            return '\0';
        }
    } 
    #endregion   
}