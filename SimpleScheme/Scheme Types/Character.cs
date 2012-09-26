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
        public static void AsString(char c, bool quoted, StringBuilder buf)
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
        public static bool Is(Obj obj)
        {
            return obj is char;
        }

        /// <summary>
        /// Casts the object to a character.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The character.</returns>
        public static char As(Obj obj)
        {
            if (Is(obj))
            {
                return (char)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
            return ' ';
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
                .DefinePrimitive("char->integer", (args, caller) => (double)As(List.First(args)), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-alphabetic? <char>)</r4rs>
                .DefinePrimitive("char-alphabetic?", (args, caller) => SchemeBoolean.Truth(char.IsLetter(As(List.First(args)))), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci<=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) <= 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci<? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci<?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) < 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) == 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci>=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) >= 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-ci>? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci>?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), true) > 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-downcase <char>)</r4rs>
                .DefinePrimitive("char-downcase", (args, caller) => As(char.ToLower(As(List.First(args)))), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-lower-case? <letter>)</r4rs>
                .DefinePrimitive("char-lower-case?", (args, caller) => SchemeBoolean.Truth(char.IsLower(As(List.First(args)))), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-numeric? <char>)</r4rs>
                .DefinePrimitive("char-numeric?", (args, caller) => SchemeBoolean.Truth(char.IsDigit(As(List.First(args)))), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-upcase <char>)</r4rs>
                .DefinePrimitive("char-upcase", (args, caller) => As(char.ToUpper(As(List.First(args)))), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-upper-case? <letter>)</r4rs>
                .DefinePrimitive("char-upper-case?", (args, caller) => SchemeBoolean.Truth(char.IsUpper(As(List.First(args)))), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char-chitespace? <char>)</r4rs>
                .DefinePrimitive("char-whitespace?", (args, caller) => SchemeBoolean.Truth(char.IsWhiteSpace(As(List.First(args)))), 1, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char<=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char<=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) <= 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char<? <char1> <char2>)</r4rs>
                .DefinePrimitive("char<?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) < 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) == 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char>=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char>=?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) >= 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char>? <char1> <char2>)</r4rs>
                .DefinePrimitive("char>?", (args, caller) => SchemeBoolean.Truth(Compare(List.First(args), List.Second(args), false) > 0), 2, Primitive.ValueType.Char)
                //// <r4rs section="6.6">(char? <obj>)</r4rs>
                .DefinePrimitive("char?", (args, caller) => SchemeBoolean.Truth(Is(List.First(args))), 1, Primitive.ValueType.Obj);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Compares two characters.
        /// </summary>
        /// <param name="obj1">The first char.</param>
        /// <param name="obj2">The second char.</param>
        /// <param name="caseInsensitive">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if x is before y, positive if x is after y, 
        /// or 0 if they are the same.</returns>
        private static int Compare(Obj obj1, Obj obj2, bool caseInsensitive)
        {
            char char1 = As(obj1);
            char char2 = As(obj2);
            return caseInsensitive ? char.ToLower(char1) - char.ToLower(char2) : char1 - char2;
        }
        #endregion
    }
}