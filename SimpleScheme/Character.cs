// <copyright file="Character.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Represents a scheme character.
    /// </summary>
    public class Character : ListPrimitives
    {
        /// <summary>
        /// Define the character primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
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
                .DefinePrimitive("char?", (caller, args) => SchemeBoolean.Truth(First(args) is char), 1);
        }

        /// <summary>
        /// Convert an object containing a character into the character.
        /// </summary>
        /// <param name="c">The object containing the char.</param>
        /// <returns>The character it contains.</returns>
        public static char Chr(Obj c)
        {
            if (!(c is char))
            {
                return Chr(ErrorHandlers.Error("Expected a char, got: " + c));
            }

            return (char)c;
        }

        /// <summary>
        /// Compares two characters.
        /// </summary>
        /// <param name="x">The first char.</param>
        /// <param name="y">The second char.</param>
        /// <param name="ci">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if x is before y, positive if x is after y, 
        /// or 0 if they are the same.</returns>
        public static int ChrCompare(Obj x, Obj y, bool ci)
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
    }
}