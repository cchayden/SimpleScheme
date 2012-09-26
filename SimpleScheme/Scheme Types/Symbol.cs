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
    public class Symbol : Printable, Cleanable
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

        /// <summary>
        /// The position within the binding environment for the symbol.
        /// </summary>
        private int pos;

        /// <summary>
        /// The number of lexical stsps out the smybol is bound.
        /// </summary>
        private int level;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Symbol class.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        private Symbol(string name)
        {
            this.name = name;
            this.pos = -1;
            this.level = -1;
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

        /// <summary>
        /// Gets a value indicating whether a symbol have been located in an environment.
        /// </summary>
        public bool Located
        {
            get { return this.pos != -1; }
        }

        /// <summary>
        /// Gets or sets the position in the environment the symbol appears.
        /// </summary>
        public int Pos
        {
            get { return this.pos; }
            set { this.pos = value; }
        }

        /// <summary>
        /// Gets or sets the number of lexical levels out that the symbol apears in an environment.
        /// </summary>
        public int Level
        {
            get { return this.level; }
            set { this.level = value; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Test two symbols for equality.
        /// </summary>
        /// <param name="obj1">The first symbol.</param>
        /// <param name="obj2">The second symbol.</param>
        /// <returns>True if they are both symbols and represent the same symbol.</returns>
        public static SchemeBoolean Equal(Obj obj1, Obj obj2)
        {
            if (!obj1.IsSymbol() || !obj2.IsSymbol())
            {
                return SchemeBoolean.False;
            }

            return obj1.ToString() == obj2.ToString() ? SchemeBoolean.True : SchemeBoolean.False;
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
                .DefinePrimitive(
                    New("string->symbol"), 
                    (args, caller) => New(SchemeString.AsString(args.First())), 
                    1, 
                    Primitive.ValueType.String)
                //// <r4rs section="6.4">(symbol? <obj>)</r4rs>
                .DefinePrimitive(
                    New("symbol?"), 
                    (args, caller) => SchemeBoolean.Truth(args.First().IsSymbol()), 
                    1, 
                    Primitive.ValueType.Obj);
        }
        #endregion

        #region Public Methods

        /// <summary>
        /// Set the environment location of this symbol.
        /// </summary>
        /// <param name="p">The position in the environment.</param>
        /// <param name="lev">The environment level -- the number of lexical steps out.</param>
        public void Locate(int p, int lev)
        {
            this.pos = p;
            this.level = lev;
        }

        /// <summary>
        /// Write the symbol to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.name);
        }

        /// <summary>
        /// Clear the cached environment information for the symbol
        /// </summary>
        public void Clean()
        {
            this.pos = -1;
            this.level = -1;
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