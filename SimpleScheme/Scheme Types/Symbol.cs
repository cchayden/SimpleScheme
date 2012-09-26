﻿// <copyright file="Symbol.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// Handles scheme symbols.
    /// A symbol is represented by an object that contains the synbol name
    /// as a string.
    /// Symbols are immutable.
    /// </summary>
    public class Symbol : SchemeObject, ICleanable, IEquatable<Symbol>
    {
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

        /// <summary>
        /// Initializes a new instance of the Symbol class.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        /// <param name="lineNumber">The line number where the symbol was read.</param>
        private Symbol(string name, int lineNumber) : base(lineNumber)
        {
            this.name = name;
            this.pos = -1;
            this.level = -1;
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

        #region New
        /// <summary>
        /// Converts a string into a Smybol.
        /// </summary>
        /// <param name="name">The string.</param>
        /// <returns>The corresponding Symbol.</returns>
        public static implicit operator Symbol(string name)
        {
            return New(name);
        }

        /// <summary>
        /// Initializes a new instance of the Symbol class.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        /// <returns>A new Smybol.</returns>
        public static Symbol New(string name)
        {
            return new Symbol(name);
        }

        /// <summary>
        /// Initializes a new instance of the Symbol class, with a line number.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        /// <param name="lineNumber">The line number where the symbol was read.</param>
        /// <returns>A new Smybol.</returns>
        public static Symbol New(string name, int lineNumber)
        {
            return new Symbol(name, lineNumber);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Test two symbols for equality.
        /// </summary>
        /// <param name="obj1">The first symbol.</param>
        /// <param name="obj2">The second symbol.</param>
        /// <returns>True if they are both symbols and represent the same symbol.</returns>
        public static SchemeBoolean Equal(Symbol obj1, SchemeObject obj2)
        {
            if (!(obj2 is Symbol))
            {
                return false;
            }

            return obj1.ToString() == obj2.ToString();
        }

        /// <summary>
        /// Provide the symbol as a string.
        /// Used to avoid the overhead of virtual dispatch when we know we have a Symbol.
        /// </summary>
        /// <param name="sym">The symbol.</param>
        /// <returns>The string contained in the Symbol.</returns>
        public static string ToString(Symbol sym)
        {
            return sym.name;
        }
        #endregion

        #region Equality
        /// <summary>
        /// Provide our own version of the Equals method.
        /// Two symbols are equal if they have the same underlying name.
        /// This ignores the differences in pos/level.
        /// These can be thought of as hints used when the symbol is looked up, but do not
        ///   make it a different symbol.
        /// The pos/level properties do make it dangerous to cache and share symbols between different
        ///   occurrances in the program text.
        /// </summary>
        /// <param name="other">The other object.</param>
        /// <returns>True if they have the same symbol name.</returns>
        public override bool Equals(object other)
        {
            if (!(other is Symbol))
            {
                return false;
            }

            return this.Equals((Symbol)other);
        }

        /// <summary>
        /// Compares two Symbol values by comparing their underlying name.
        /// </summary>
        /// <param name="other">The other Symbol.</param>
        /// <returns>True if they have the same name.</returns>
        public bool Equals(Symbol other)
        {
            return this.name == other.name;
        }

        /// <summary>
        /// The hash code is the name's hash code.
        /// </summary>
        /// <returns>The hash code.</returns>
        public override int GetHashCode()
        {
            return this.name.GetHashCode();
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the symbol primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            env
                .DefinePrimitive(
                    new Symbol("string->symbol"), 
                    new[] { "6.4", "(string->symbol <string>)" }, 
                    (args, caller) => new Symbol(SchemeString.AsString(First(args))), 
                    1, 
                    Primitive.ArgType.String)
                .DefinePrimitive(
                    new Symbol("symbol?"), 
                    new[] { "6.4", "(symbol? <obj>)" }, 
                    (args, caller) => SchemeBoolean.Truth(First(args) is Symbol), 
                    1, 
                    Primitive.ArgType.Obj);
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
        /// Describe a symbol by returning its value.
        /// </summary>
        /// <returns>The symbol as a string.</returns>
        public override string Describe()
        {
            return this.ToString();
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
}