// <copyright file="Character.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Handles a scheme character.
    /// Scheme characters are represented as .NET char objects.
    /// </summary>
    public class Character : IPrintable, ISchemeType
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

        #region Private Static
        /// <summary>
        /// Tests whether two Characters are equal.
        /// </summary>
        /// <param name="obj1">The first object (must be a scheme Character).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both the same character.</returns>
        public static SchemeBoolean Equal(Obj obj1, Obj obj2)
        {
            if (!obj2.IsCharacter())
            {
                return SchemeBoolean.False;
            }

            var char1 = obj1.AsCharacter();
            var char2 = obj2.AsCharacter();
            if (char1.c != char2.C)
            {
                return SchemeBoolean.False;
            }

            return SchemeBoolean.True;
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
            return obj is Character;
        }

        /// <summary>
        /// Create a new Character object;
        /// </summary>
        /// <param name="c">The character that this represents.</param>
        /// <returns>A new Character.</returns>
        public static Character New(char c)
        {
            return new Character(c);
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
                        Symbol.New("char->integer"), 
                        (args, caller) => Number.New(args.First().AsCharacter().C), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-alphabetic? <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-alphabetic?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsLetter(args.First().AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("char-ci<=?"), 
                    (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) <= 0), 
                    2, 
                    TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci<?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) < 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) == 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci>=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) >= 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci>?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), true) > 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-downcase <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-downcase"), 
                        (args, caller) => New(char.ToLower(args.First().AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-lower-case? <letter>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-lower-case?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsLower(args.First().AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-numeric? <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-numeric?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsDigit(args.First().AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-upcase <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-upcase"), 
                        (args, caller) => New(char.ToUpper(args.First().AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-upper-case? <letter>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-upper-case?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsUpper(args.First().AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char-chitespace? <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-whitespace?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsWhiteSpace(args.First().AsCharacter().C)), 
                        1, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                       Symbol.New("char<=?"), 
                       (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) <= 0), 
                       2, 
                       TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char<?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) < 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) == 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char>=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) >= 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char>?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare(args.First(), args.Second(), false) > 0), 
                        2, 
                        TypePrimitives.ValueType.Char)
                //// <r4rs section="6.6">(char? <obj>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char?"), 
                        (args, caller) => SchemeBoolean.Truth(args.First().IsCharacter()), 
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
        private static int Compare(Obj char1, Obj char2, bool caseInsensitive)
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
        /// Tests whether to given object is a scheme character.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme character.</returns>
        public static bool IsCharacter(this Obj obj)
        {
            return Character.Is(obj);
        }

        /// <summary>
        /// Convert to a character.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding character.</returns>
        public static Character AsCharacter(this Obj x)
        {
            if (Character.Is(x))
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
        public static char AsChar(this Obj x)
        {
            if (Character.Is(x))
            {
                return ((Character)x).C;
            }

            ErrorHandlers.TypeError(typeof(Character), x);
            return '\0';
        }
    } 
    #endregion   
}