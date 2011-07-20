// <copyright file="Symbol.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Handles scheme symbols.
    /// Smybols are represented by .NET strings.
    /// </summary>
    public class Symbol
    {
        #region Constructors
        /// <summary>
        /// Prevents a default instance of the Symbol class from being created.
        /// </summary>
        private Symbol()
        {
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Check that the object is a symbol.
        /// Symbols are represented by .NET strings.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding symbol.</returns>
        public static string Sym(Obj x)
        {
            if (TypePrimitives.IsSymbol(x))
            {
                return (string)x;
            }

            ErrorHandlers.TypeError(TypePrimitives.SymbolName, x);
            return null;
        }

        /// <summary>
        /// Create a new symbol from an object.
        /// The object must be a schme string.
        /// </summary>
        /// <param name="obj">The object, which must be a scheme string.</param>
        /// <returns>The symbol.</returns>
        public static string New(Obj obj)
        {
            return new string(SchemeString.Str(obj));
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the symbol primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// <r4rs section="6.4">(string->symbol <string>)</r4rs>
                .DefinePrimitive("string->symbol", (args, caller) => New(List.First(args)), 1)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive("symbol?", (args, caller) => SchemeBoolean.Truth(TypePrimitives.IsSymbol(List.First(args))), 1);
        }
        #endregion
    }
}