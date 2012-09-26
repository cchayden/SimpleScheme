// <copyright file="Symbol.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Handles scheme symbols.
    /// A symbol is represented by an object that contains the synbol name
    /// as a string.
    /// Symbols are immutable.
    /// </summary>
    public class Symbol : SchemeObject, IEquatable<Symbol>
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

        /// <summary>
        /// Marks a special form: a primitive that does not evaluate its arguments.
        /// If the symbol is a special form, this is the action to take.
        /// </summary>
        private SpecialAction specialForm;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Symbol class.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        private Symbol(string name)
        {
            Contract.Requires(name != null);
            this.name = name;
            this.pos = -1;
            this.level = -1;
            specialForm = null;
        }

        /// <summary>
        /// Initializes a new instance of the Symbol class.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        /// <param name="lineNumber">The line number where the symbol was read.</param>
        private Symbol(string name, int lineNumber) : base(lineNumber)
        {
            Contract.Requires(name != null);
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
            get
            {
                Contract.Ensures(Contract.Result<string>() != null);
                return this.name;
            }
        }

        /// <summary>
        /// Gets a value indicating whether a symbol have been located in an environment.
        /// </summary>
        internal bool Located
        {
            get { return this.pos != -1; }
        }

        /// <summary>
        /// Gets or sets the position in the environment the symbol appears.
        /// </summary>
        internal int Pos
        {
            get
            {
                return this.pos;
            }
            set
            {
                Contract.Requires(value >= 0);
                this.pos = value;
            }
        }

        /// <summary>
        /// Gets or sets the number of lexical levels out that the symbol apears in an environment.
        /// </summary>
        internal int Level
        {
            get
            {
                return this.level;
            }
            set
            {
                Contract.Requires(value >= 0);
                this.level = value;
            }
        }

        /// <summary>
        /// Gets or sets a the action for a special form.
        /// </summary>
        internal SpecialAction SpecialForm
        {
            get { return this.specialForm; }
            set { this.specialForm = value; }
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
            Contract.Requires(name != null);
            Contract.Ensures(Contract.Result<Symbol>() != null);
            return New(name);
        }

        /// <summary>
        /// Initializes a new instance of the Symbol class.
        /// </summary>
        /// <param name="name">The value of the symbol.</param>
        /// <returns>A new Symbol.</returns>
        public static Symbol New(string name)
        {
            Contract.Requires(name != null);
            Contract.Ensures(Contract.Result<Symbol>() != null);
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
            Contract.Requires(name != null);
            Contract.Ensures(Contract.Result<Symbol>() != null);
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
            Contract.Requires(obj1 != null);
            Contract.Requires(obj2 != null);
            if (!(obj2 is Symbol))
            {
                return false;
            }

            return obj1.ToString() == obj2.ToString();
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
            Contract.Assume(other != null);
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

        #region Public Methods
        /// <summary>
        /// Provide the symbol as a string.
        /// </summary>
        /// <returns>The symbol as a string.</returns>
        public override string ToString()
        {
            return this.name;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the symbol primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            Contract.Requires(primEnv != null);
            primEnv
                .DefinePrimitive(
                    new Symbol("string->symbol"), 
                    new[] { "6.4", "(string->symbol <string>)" }, 
                    (args, env, caller) => new Symbol(SchemeString.AsString(First(args))), 
                    new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                    new Symbol("symbol?"), 
                    new[] { "6.4", "(symbol? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(First(args) is Symbol), 
                    new ArgsInfo(1, ArgType.Obj));
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Clear the cached environment information for the symbol
        /// </summary>
        internal override void Clean()
        {
            this.pos = -1;
            this.level = -1;
        }

        /// <summary>
        /// Set the environment location of this symbol.
        /// </summary>
        /// <param name="p">The position in the environment.</param>
        /// <param name="lev">The environment level -- the number of lexical steps out.</param>
        internal void SetLocation(int p, int lev)
        {
            this.pos = p;
            this.level = lev;
        }

        /// <summary>
        /// Describe a symbol by returning its value.
        /// </summary>
        /// <returns>The symbol as a string.</returns>
        internal override string Describe()
        {
            return this.ToString();
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.name != null);
        }
        #endregion
    }
}