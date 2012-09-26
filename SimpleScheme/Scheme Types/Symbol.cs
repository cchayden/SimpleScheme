// <copyright file="Symbol.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Handles scheme symbols.
    /// A symbol is represented by an object that contains the synbol name
    /// as a string.
    /// Symbols are immutable.
    /// </summary>
    public class Symbol : Printable
    {
        #region Constants
        /// <summary>
        /// The printable name of the symbol type.
        /// </summary>
        public const string Name = "symbol";
        #endregion

        #region Fields
        /// <summary>
        /// The symbol's value, as a string.
        /// </summary>
        private readonly string name;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Symbol class.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        private Symbol(string name)
        {
            this.name = name;
        }
        #endregion

        #region Public Static Properties
        /// <summary>
        /// Gets the scheme type name.
        /// </summary>
        public static string TypeName
        {
            get { return "Symbol"; }
        }
        #endregion

        #region Public Properties
        /// <summary>
        /// Gets the symbol name.
        /// </summary>
        public string SymbolName
        {
            get { return this.name; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Test two symbols for equality.
        /// </summary>
        /// <param name="obj1">The first symbol.</param>
        /// <param name="obj2">The second symbol.</param>
        /// <returns>True if they are both symbols and represent the same symbol.</returns>
        public static bool Equal(Obj obj1, Obj obj2)
        {
            if (!obj1.IsSymbol() || !obj2.IsSymbol())
            {
                return false;
            }

            return obj1.ToString() == obj2.ToString();
        }

        /// <summary>
        /// Create a new symbol with the given name.
        /// </summary>
        /// <param name="name">The symbol name.</param>
        /// <returns>The new symbol</returns>
        public static Symbol New(string name)
        {
            return new Symbol(name);
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
                .DefinePrimitive("string->symbol", (args, caller) => New(SchemeString.AsString(args.First())), 1, Primitive.ValueType.String)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive("symbol?", (args, caller) => SchemeBoolean.Truth(args.First().IsSymbol()), 1, Primitive.ValueType.Obj);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the symbol to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.name);
        }

        /// <summary>
        /// Provide the symbol as a string.
        /// </summary>
        /// <returns>The symbol as a string.</returns>
        public override string ToString()
        {
            return this.name;
        }

        #endregion
    }

    /// <summary>
    /// Extension class for Symbol
    /// </summary>
    public static class SymbolExtensions
    {
        /// <summary>
        /// Tests whether to given object is a scheme symbol.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme symbol.</returns>
        public static bool IsSymbol(this Obj obj)
        {
            return obj is Symbol;
        }

        /// <summary>
        /// Check that the object is a symbol.
        /// The actual symbol name is contained in the Symbol.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding symbol.</returns>
        public static Symbol AsSymbol(this Obj x)
        {
            if (x.IsSymbol())
            {
                return (Symbol)x;
            }

            ErrorHandlers.TypeError(Symbol.Name, x);
            return null;
        }
    }
}