// <copyright file="Character.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Handles a scheme character.
    /// Scheme characters are represented as .NET char objects.
    /// </summary>
    public class Character : ListPrimitives
    {
        #region Public Static Methods
        /// <summary>
        /// Checks that the object is a character.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The character.</returns>
        public static char Chr(Obj obj)
        {
            if (obj is char)
            {
                return (char)obj;
            }

            return Chr(ErrorHandlers.TypeError("char", obj));
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the character primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            env
                //// <r4rs section="6.6">(char->integer <char>)</r4rs>
                .DefinePrimitive("char->integer", (args, caller) => (double)Chr(First(args)), 1)
                //// <r4rs section="6.6">(char-alphabetic? <char>)</r4rs>
                .DefinePrimitive("char-alphabetic?", (args, caller) => SchemeBoolean.Truth(char.IsLetter(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-ci<=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci<=?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) <= 0), 2)
                //// <r4rs section="6.6">(char-ci<? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci<?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) < 0), 2)
                //// <r4rs section="6.6">(char-ci=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci=?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) == 0), 2)
                //// <r4rs section="6.6">(char-ci>=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci>=?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) >= 0), 2)
                //// <r4rs section="6.6">(char-ci>? <char1> <char2>)</r4rs>
                .DefinePrimitive("char-ci>?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), true) > 0), 2)
                //// <r4rs section="6.6">(char-downcase <char>)</r4rs>
                .DefinePrimitive("char-downcase", (args, caller) => Chr(char.ToLower(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-lower-case? <letter>)</r4rs>
                .DefinePrimitive("char-lower-case?", (args, caller) => SchemeBoolean.Truth(char.IsLower(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-numeric? <char>)</r4rs>
                .DefinePrimitive("char-numeric?", (args, caller) => SchemeBoolean.Truth(char.IsDigit(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-upcase <char>)</r4rs>
                .DefinePrimitive("char-upcase", (args, caller) => Chr(char.ToUpper(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-upper-case? <letter>)</r4rs>
                .DefinePrimitive("char-upper-case?", (args, caller) => SchemeBoolean.Truth(char.IsUpper(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char-chitespace? <char>)</r4rs>
                .DefinePrimitive("char-whitespace?", (args, caller) => SchemeBoolean.Truth(char.IsWhiteSpace(Chr(First(args)))), 1)
                //// <r4rs section="6.6">(char<=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char<=?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) <= 0), 2)
                //// <r4rs section="6.6">(char<? <char1> <char2>)</r4rs>
                .DefinePrimitive("char<?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) < 0), 2)
                //// <r4rs section="6.6">(char=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char=?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) == 0), 2)
                //// <r4rs section="6.6">(char>=? <char1> <char2>)</r4rs>
                .DefinePrimitive("char>=?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) >= 0), 2)
                //// <r4rs section="6.6">(char>? <char1> <char2>)</r4rs>
                .DefinePrimitive("char>?", (args, caller) => SchemeBoolean.Truth(ChrCompare(First(args), Second(args), false) > 0), 2)
                //// <r4rs section="6.6">(char? <obj>)</r4rs>
                .DefinePrimitive("char?", (args, caller) => SchemeBoolean.Truth(First(args) is char), 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a scheme character.</returns>
        internal static bool IsType(Obj obj)
        {
            return obj is char;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string TypeName()
        {
            return "char";
        }

        /// <summary>
        /// Compares two characters.
        /// </summary>
        /// <param name="x">The first char.</param>
        /// <param name="y">The second char.</param>
        /// <param name="ci">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if x is before y, positive if x is after y, 
        /// or 0 if they are the same.</returns>
        internal static int ChrCompare(Obj x, Obj y, bool ci)
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
        #endregion
    }
}