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
    public static class Symbol
    {
        #region Constants
        /// <summary>
        /// The printable name of the symbol type.
        /// </summary>
        public const string Name = "symbol";
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme symbol.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme symbol.</returns>
        public static bool IsSymbol(Obj obj)
        {
            return obj is string;
        }

        /// <summary>
        /// Check that the object is a symbol.
        /// Symbols are represented by .NET strings.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding symbol.</returns>
        public static string AsSymbol(Obj x)
        {
            if (IsSymbol(x))
            {
                return (string)x;
            }

            ErrorHandlers.TypeError(Name, x);
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
            return new string(SchemeString.AsSchemeString(obj));
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the symbol primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                //// <r4rs section="6.4">(string->symbol <string>)</r4rs>
                .DefinePrimitive("string->symbol", (args, caller) => New(List.First(args)), 1)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive("symbol?", (args, caller) => SchemeBoolean.Truth(IsSymbol(List.First(args))), 1);
        }
        #endregion
    }

    #region Extensions
    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    public static partial class Extensions
    {
        /// <summary>
        /// Write the symbol to the string builder.
        /// </summary>
        /// <param name="sym">The symbol.</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this string sym, bool quoted, StringBuilder buf)
        {
            buf.Append(sym);
        }
    }
    #endregion
}