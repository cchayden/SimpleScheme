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
            if (IsType(x))
            {
                return (string)x;
            }

            ErrorHandlers.TypeError(TypeName(), x);
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
                .DefinePrimitive("string->symbol", (args, caller) => New(First(args)), 1)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive("symbol?", (args, caller) => SchemeBoolean.Truth(IsType(First(args))), 1);
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
        /// <param name="obj">The symbol name.</param>
        /// <param name="quoted">Whether to quote the string.</param>
        /// <param name="buf">Accumulate the result into here.</param>
        internal static void AsString(Obj obj, bool quoted, StringBuilder buf)
        {
            buf.Append((string)obj);
        }
        #endregion
    }
}