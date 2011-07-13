// <copyright file="Symbol.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Handles scheme symbols.
    /// Smybols are represented by .NET strings.
    /// </summary>
    public class Symbol : ListPrimitives
    {
        #region Public Static Methods
        /// <summary>
        /// Check that the object is a symbol.
        /// Symbols are represented by .NET strings.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding symbol.</returns>
        public static string Sym(Obj x)
        {
            if (x is string)
            {
                return (string)x;
            }

            return Sym(ErrorHandlers.TypeError("symbol", x));
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the symbol primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            env
                //// <r4rs section="6.4">(symbol->string <symbol>)</r4rs>
                .DefinePrimitive("symbol->string", (args, caller) => SchemeString.MakeString(Sym(First(args))), 1)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive("symbol?", (args, caller) => SchemeBoolean.Truth(First(args) is string), 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a symbol.</returns>
        internal static bool IsType(Obj obj)
        {
            return obj is string;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string TypeName()
        {
            return "symbol";
        }

        /// <summary>
        /// Convert symbol into string.
        /// </summary>
        /// <param name="str">The symbol name.</param>
        /// <param name="quoted">Whether to quote the string.</param>
        /// <param name="buf">Accumulate the result into here.</param>
        internal static void AsString(string str, bool quoted, StringBuilder buf)
        {
            buf.Append(str);
        }
        #endregion
    }
}