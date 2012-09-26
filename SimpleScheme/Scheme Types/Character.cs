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
    public static class Character
    {
        #region Constants
        /// <summary>
        /// The printable name of the scheme character type.
        /// </summary>
        public const string Name = "character";
        #endregion

        #region Accessors
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Write the character to the string builder.
        /// </summary>
        /// <param name="c">The character.</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void PrintString(this char c, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("#\\");
            }

            if (c == ' ')
            {
                buf.Append("space");
            }
            else
            {
                buf.Append(c);
            }
        }

        /// <summary>
        /// Tests whether to given object is a scheme character.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme character.</returns>
        public static bool IsCharacter(this Obj obj)
        {
            return obj is char;
        }

        /// <summary>
        /// Casts the object to a character.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The character.</returns>
        public static char AsCharacter(this Obj obj)
        {
            if (obj.IsCharacter())
            {
                return (char)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return ' ';
        }

        /// <summary>
        /// Create a new Character object;
        /// Since Character is just char, nothing to do.
        /// </summary>
        /// <param name="c">The character that this represents.</param>
        /// <returns>A new Character.</returns>
        public static char New(char c)
        {
            return c;
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
                        (args, caller) => (int)(char)args.First(), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-alphabetic? <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-alphabetic?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsLetter((char)args.First())), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("char-ci<=?"), 
                    (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), true) <= 0), 
                    2, 
                    Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci<?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), true) < 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), true) == 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci>=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), true) >= 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-ci>?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), true) > 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-downcase <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-downcase"), 
                        (args, caller) => char.ToLower((char)args.First()), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-lower-case? <letter>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-lower-case?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsLower((char)args.First())), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-numeric? <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-numeric?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsDigit((char)args.First())), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-upcase <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-upcase"), 
                        (args, caller) => char.ToUpper((char)args.First()).AsCharacter(), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-upper-case? <letter>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-upper-case?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsUpper((char)args.First())), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-chitespace? <char>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char-whitespace?"), 
                        (args, caller) => SchemeBoolean.Truth(char.IsWhiteSpace((char)args.First())), 
                        1, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char<=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                       Symbol.New("char<=?"), 
                       (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), false) <= 0), 
                       2, 
                       Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char<? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char<?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), false) < 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), false) == 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char>=? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char>=?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), false) >= 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char>? <char1> <char2>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char>?"), 
                        (args, caller) => SchemeBoolean.Truth(Compare((char)args.First(), (char)args.Second(), false) > 0), 
                        2, 
                        Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char? <obj>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("char?"), 
                        (args, caller) => SchemeBoolean.Truth(args.First().IsCharacter()), 
                        1, 
                        Primitive.ValueType.Obj);
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
        private static int Compare(char char1, char char2, bool caseInsensitive)
        {
            return caseInsensitive ? char.ToLower(char1) - char.ToLower(char2) : char1 - char2;
        }
        #endregion
    }
}