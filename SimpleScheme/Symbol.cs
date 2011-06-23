// <copyright file="Symbol.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Represents a scheme symbol.
    /// </summary>
    public class Symbol : ListPrimitives
    {
        /// <summary>
        /// Define the symbol primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            env
                //// <r4rs section="6.4">(symbol->string <symbol>)</r4rs>
                .DefinePrimitive("symbol->string", (caller, args) => SchemeString.MakeString(Sym(First(args))), 1)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive("symbol?", (caller, args) => SchemeBoolean.Truth(First(args) is string), 1);
        }

        /// <summary>
        /// Convert symbol into string.
        /// </summary>
        /// <param name="str">The symbol name.</param>
        /// <param name="quoted">Whether to quote the string.</param>
        /// <param name="buf">Accumulate the result into here.</param>
        public static void AsString(string str, bool quoted, StringBuilder buf)
        {
            buf.Append(str);
        }

        /// <summary>
        /// Turn an object that is a symbol into a string.
        /// Symbols are represented by .NET strings.
        /// It is stored as one already, so just verify that this is a symbol.
        /// </summary>
        /// <param name="x">The symbol.</param>
        /// <returns>The corresponding string.</returns>
        public static string Sym(object x)
        {
            if (x is string)
            {
                return (string)x;
            }

            return Sym(ErrorHandlers.Error("Expected a symbol, got: " + x));
        }
    }
}