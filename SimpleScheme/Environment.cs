#define OLD
// <copyright file="Environment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;
    using System.Text;

    /// <summary>
    /// Represents the interpreter environment.
    /// The environment is part of a chain, made up of the Caller links, that represents the dynamic context.
    /// At the bottom of the chain is the global environment, created by the single-arg constructor.
    /// Each link in the chain contains a symbol table of the bindings at that level.
    /// Each lookup searches down the symbol tables in the chain, from the top to the bottom.
    /// </summary>
    public class Environment : IEnvironment
    {
        #region Fields
        /// <summary>
        /// The interpreter.
        /// </summary>
        private readonly Interpreter interp;

        /// <summary>
        /// The lexically enclosing environment.
        /// </summary>
        private readonly Environment lexicalParent;

        /// <summary>
        /// Stores the name/value pairs that make up the symbol table.
        /// </summary>
        private readonly List<NameValuePair> symbolTable;

        /// <summary>
        /// Flags the primitive environment.
        /// </summary>
        private readonly bool primitive;

        /// <summary>
        /// Flags the empty environment.
        /// </summary>
        private readonly bool empty;

        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// This is used to create the global environment.
        /// When we refer to "parent" we mean the enclosing lexical environment.
        /// </summary>
        /// <param name="lexicalParent">The lexical parent environment.</param>
        /// <param name="interp">The interpreter.</param>
        public Environment(Environment lexicalParent, Interpreter interp)
        {
            Contract.Requires(lexicalParent != null);
            Contract.Requires(interp != null);
            this.interp = interp;
            this.lexicalParent = lexicalParent;
            this.symbolTable = new List<NameValuePair>();
            this.primitive = this.empty = false;
            interp.IncrementCounter(this.GetType().Name + ":ctor");
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// </summary>
        /// <param name="lexicalParent">The lexically enclosing environment.</param>
        /// <returns>The new environment.</returns>
        public Environment(Environment lexicalParent)
            : this(lexicalParent, lexicalParent.Interp)
        {
            Contract.Requires(lexicalParent != null);
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// Start out with a set of variable bindings and a lexical parent environment.
        /// The initial variable bindings are the formal parameters and the corresponding argument values.
        /// </summary>
        /// <param name="lexicalParent">The lexical parent environment.</param>
        /// <param name="formals">A list of variable names.</param>
        /// <param name="inits">The values for these variables.</param>
        public Environment(Environment lexicalParent, SchemeObject formals, SchemeObject inits)
        {
            Contract.Requires(lexicalParent != null);
            Contract.Requires(formals != null);
            Contract.Requires(inits != null);
            this.interp = lexicalParent.Interp;
            this.lexicalParent = lexicalParent;
            this.symbolTable = new List<NameValuePair>(List.ListLength(formals));
            this.primitive = this.empty = false;
            this.AddSymbolList(formals, inits);
            interp.IncrementCounter(this.GetType().Name + ":ctor");
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// This is used to create the primitive environment.
        /// </summary>
        /// <param name="primitive">If true, this creates a primitive environment.
        /// Otherwise, creates an empty environment.</param>
        public Environment(bool primitive)
        {
            this.lexicalParent = null;
            this.interp = null;
            this.symbolTable = new List<NameValuePair>();
            this.primitive = primitive;
            this.empty = !primitive;
        }

        #endregion

        #region Accessors
        /// <summary>
        /// Gets the interpreter used to hold the global context.
        /// The bottom environment holds the interpreter, but each one copies from its
        ///   lexical parent, so that it can be accessed directly.
        /// This field is written only in the constructor -- it is never modified.
        /// </summary>
        internal Interpreter Interp
        {
            get
            {
                Contract.Ensures(Contract.Result<Interpreter>() != null);
#if Check
                if (this.interp == null)
                {
                    ErrorHandlers.InternalError("Interp is null");
                }
#endif
                return this.interp;
            }
        }

        /// <summary>
        /// Gets the interpreter, which may be null.
        /// </summary>
        internal Interpreter InterpOrNull
        {
            get
            {
                return this.interp;
            }
        }

        /// <summary>
        /// Gets the lexical parent environment.
        /// </summary>
        internal Environment LexicalParent
        {
            get
            {
                Contract.Ensures(Contract.Result<Environment>() != null);
#if Check
                if (this.lexicalParent == null)
                {
                    ErrorHandlers.InternalError("LexicalParent is null");
                }
#endif
                return this.lexicalParent;
            }
        }

        /// <summary>
        /// Gets the lexical parent environment.
        /// </summary>
        internal Environment LexicalParentOrNull
        {
            get { return this.lexicalParent; }
        }
        #endregion

        #region Interface Methods

        /// <summary>
        /// Add a new definition into the environment.
        /// Creates a new symbol through public interface instead of through parser.
        /// </summary>
        /// <param name="var">A variable to add to the environment.</param>
        /// <param name="val">The value of that variable.</param>
        public void Define(string var, SchemeObject val)
        {
            try
            {
                this.Define(new Symbol(var), val);
            }
            catch (ErrorHandlers.SchemeException)
            {
            }
        }

        /// <summary>
        /// Set the value of a variable in the environment.
        /// Var is the variable name.
        /// Searches the chain of environments looking for a binding.
        /// </summary>
        /// <param name="var">The variable name.</param>
        /// <param name="val">The new value for the variable.</param>
        public void Set(string var, SchemeObject val)
        {
            try
            {
                this.Set(new Symbol(var), val);
            }
            catch (ErrorHandlers.SchemeException)
            {
            }
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// </summary>
        /// <param name="var">The name of the variable to look up.  Must name a symbol.</param>
        /// <returns>The value bound to the variable.  If the symbol is not found, returns null.</returns>
        public SchemeObject Lookup(string var)
        {
            try
            {
                return this.Lookup(new Symbol(var));
            }
            catch (ErrorHandlers.SchemeException)
            {
                return null;
            }
        }

        /// <summary>
        /// The default way to display environments during debugging.
        /// </summary>
        /// <returns>The environment as a string.</returns>
        public override string ToString()
        {
            return "Environment \n" + this.Dump();
        }

        #endregion

        #region Internal Methods
        /// <summary>
        /// Add a new definition into the environment.
        /// If a procedure is being added, set its name.
        /// </summary>
        /// <param name="var">This is a variable to add to the environment.</param>
        /// <param name="val">This is the value of that variable.</param>
        internal void Define(Symbol var, SchemeObject val)
        {
            Contract.Requires(var != null);
            Contract.Requires(val != null);
            this.AddSymbol(var, val);

            if (val is Procedure)
            {
                ((Procedure)val).ProcedureName = var.SymbolName;
            }
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// This is the single most expensive operation in the whole interpreter.
        /// Is there some way to make this faster?
        /// </summary>
        /// <param name="var">The name of the variable to look up.  Must be a symbol.</param>
        /// <returns>The value bound to the variable.</returns>
        internal SchemeObject Lookup(Symbol var)
        {
            Contract.Requires(var != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            Environment env = this;

            // search down the chain of environments for a definition
            int level = 0;
            if (var.Located)
            {
                // located -- just step out to the right level and get the value
                while (level < var.Level)
                {
                    env = env.LexicalParent;
                    level++;
                }

                return env.LookupLocatedSymbol(var);
            }

            // not located -- search for it
            while (env != null)
            {
                SchemeObject val = env.LookupSymbol(var, level);
                if (val != null)
                {
                    return val;
                }

                // if we have not found anything yet, look in the parent
                env = env.LexicalParentOrNull;
                level++;
            }

            ErrorHandlers.SemanticError(string.Format(@"Unbound variable: ""{0}""", var.SymbolName));
            return null;
        }

        /// <summary>
        /// Add or update a symbol table entry at this level.
        /// </summary>
        /// <param name="symbol">The symbol to update.</param>
        /// <param name="val">The new value.</param>
        internal void Update(SchemeObject symbol, SchemeObject val)
        {
            Contract.Requires(symbol != null);
            Contract.Requires(val != null);
            if (!(symbol is Symbol))
            {
                ErrorHandlers.SemanticError(
                    string.Format(@"Attempt to update a non-symbol: ""{0}""", symbol.ToString(true)));
            }

            this.AddSymbol((Symbol)symbol, val);
        }

        /// <summary>
        /// Set the value of a variable in the environment.
        /// Var is the variable name.
        /// Searches the chain of environments looking for a binding.
        /// Used by set!
        /// </summary>
        /// <param name="symbol">The variable name.</param>
        /// <param name="val">The new value for the variable.</param>
        internal void Set(Symbol symbol, SchemeObject val)
        {
            Contract.Requires(symbol != null);
            Contract.Requires(val != null);
            Environment env = this;

            // search down the chain of environments for a definition
            int level = 0;
            if (symbol.Located)
            {
                // located -- step to environment and update
                while (level < symbol.Level)
                {
                    env = env.LexicalParent;
                    level++;
                }

                env.UpdateLocatedSymbol(symbol, val);
                return;
            }

            // not located -- search
            while (env != null)
            {
                if (env.UpdateSymbol(symbol, val, level) != null)
                {
                    return;
                }

                // if we have not found anything yet, look in the parent
                env = env.LexicalParentOrNull;
                level++;
            }

            ErrorHandlers.SemanticError(string.Format(@"Unbound variable in set!: ""{0}""", symbol.SymbolName));
        }

        /// <summary>
        /// Increment the variable.
        /// </summary>
        /// <param name="var">The symbol naming the variable to increment.</param>
        /// <returns>The incremented value.</returns>
        internal SchemeObject Increment(SchemeObject var)
        {
            Contract.Requires(var != null);
            if (!(var is Symbol))
            {
                return
                    ErrorHandlers.SemanticError(
                        string.Format(@"Attempt to increment a non-symbol: ""{0}""", var.ToString(true)));
            }

            var symbol = (Symbol)var;
            Environment env = this;

            // search down the chain of environments for a definition
            while (env != null)
            {
                SchemeObject val = env.IncrementSymbolValue(symbol);
                if (val != null)
                {
                    return val;
                }

                // if we have not found anything yet, look in the parent
                env = env.LexicalParentOrNull;
            }

            return ErrorHandlers.SemanticError(
                string.Format(@"Unbound variable in set!: ""{0}""", symbol.SymbolName));
        }

        /// <summary>
        /// Dump the  of environments.
        /// At each level, show the symbol table.
        /// </summary>
        /// <param name="levels">The number of levels to show.</param>
        /// <param name="indent">The number of characters to indent.</param>
        /// <returns>The environment stack, as a string.</returns>
        internal string Dump(int levels, int indent)
        {
            Contract.Ensures(Contract.Result<string>() != null);
            var sb = new StringBuilder();
            Environment env = this;
            while (env != null && levels > 0)
            {
                env.DumpSymbolTable(indent, sb);
                levels--;
                if (levels > 0)
                {
                    sb.Append("-----\n ");
                }

                env = env.LexicalParentOrNull;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Dump the environment.
        /// </summary>
        internal void DumpEnv()
        {
            if (this.interp != null)
            {
                this.interp.CurrentOutputPort.WriteLine(this.Dump(100, 0));
            }
        }

        /// <summary>
        /// Dump the top level environment.
        /// </summary>
        /// <returns>The environment, as a string.</returns>
        internal string Dump()
        {
            return this.Dump(1, 0);
        }

        /// <summary>
        /// Increment the given counter.
        /// </summary>
        /// <param name="counter">The counter name.</param>
        internal void IncrementCounter(string counter)
        {
            if (this.interp != null)
            {
                this.interp.IncrementCounter(counter);
            }
        }
        #endregion

        #region Symbol Table
        /// <summary>
        /// Look up a symbol given its name.
        /// The symbol must already be located.
        /// </summary>
        /// <param name="symbol">The symbol to look up.</param>
        /// <returns>The value of the object looked up, null if not found.</returns>
        internal SchemeObject LookupLocatedSymbol(Symbol symbol)
        {
            Contract.Requires(symbol != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            lock (this.symbolTable)
            {
                Contract.Assume(symbol.Pos >= 0);
                return this.symbolTable[symbol.Pos].Value;
            }
        }

        /// <summary>
        /// Look up a symbol given its name.
        /// If the symbol is not found at this level, then return null and we will continue to search
        /// through the parent environment chain.
        /// </summary>
        /// <param name="symbol">The symbol to look up.</param>
        /// <param name="level">The level to set into the symbol</param>
        /// <returns>The value of the object looked up, null if not found.</returns>
        internal SchemeObject LookupSymbol(Symbol symbol, int level)
        {
            Contract.Requires(symbol != null);
            //// return value of null is used to indicate that symbol was not found
            lock (this.symbolTable)
            {
                var index = this.FindSymbol(symbol);
                if (index < 0)
                {
                    return null;
                }

                symbol.SetLocation(index, level);
                return this.symbolTable[index].Value;
            }
        }

        /// <summary>
        /// Add a symbol and its value to the environment.
        /// If the symbol is not defined in the environment, add it.
        /// If the symbol is already defined in the envhronment, update its value.
        /// </summary>
        /// <param name="symbol">The symbol name.</param>
        /// <param name="val">The value.</param>
        internal void AddSymbol(Symbol symbol, SchemeObject val)
        {
            Contract.Requires(symbol != null);
            Contract.Requires(val != null);
            lock (this.symbolTable)
            {
                this.AddSymbolUnlocked(symbol, val);
            }
        }

        /// <summary>
        /// Update a value in the symbol table.
        /// The symbol must have been located previously.
        /// </summary>
        /// <param name="symbol">The symbol to update.</param>
        /// <param name="val">The new value.</param>
        /// <returns>The new value, null if not found.</returns>
        internal SchemeObject UpdateLocatedSymbol(Symbol symbol, SchemeObject val)
        {
            lock (this.symbolTable)
            {
                Contract.Assume(symbol.Pos >= 0);
                this.symbolTable[symbol.Pos].Value = val;
                return val;
            }
        }

        /// <summary>
        /// Update a value in the symbol table.
        /// If the symbol is not defined, return false.
        /// Cache the symbol location.
        /// </summary>
        /// <param name="symbol">The symbol to update.</param>
        /// <param name="val">The new value.</param>
        /// <param name="level">The number of levels to travel to find the definition</param>
        /// <returns>The new value, null if not found.</returns>
        internal SchemeObject UpdateSymbol(Symbol symbol, SchemeObject val, int level)
        {
            Contract.Requires(symbol != null);
            Contract.Requires(val != null);
            lock (this.symbolTable)
            {
                var index = this.FindSymbol(symbol);
                if (index < 0)
                {
                    return null;
                }

                this.symbolTable[index].Value = val;
                symbol.SetLocation(index, level);
                return val;
            }
        }

        /// <summary>
        /// Increment the value for a symbol at this level.
        /// </summary>
        /// <param name="symbol">The symbol whose value should be incremented.  The existing value must be a number.</param>
        /// <returns>The new value, null if not found.</returns>
        internal SchemeObject IncrementSymbolValue(Symbol symbol)
        {
            Contract.Requires(symbol != null);
            lock (this.symbolTable)
            {
                var index = this.FindSymbol(symbol);
                if (index < 0)
                {
                    return null;
                }

                SchemeObject val = this.symbolTable[index].Value;
                if (!(val is Number))
                {
                    // If it is found but is not a number, then that is an error.
                    ErrorHandlers.SemanticError(
                        string.Format(@"Attempt to increment a non-number: ""{0}""", val.ToString(true)));
                    return null;
                }

                this.symbolTable[index].Value = (Number)(((Number)val).N + 1);
                return val;
            }
        }

        /// <summary>
        /// Dump the symbol table to a string for printing.
        /// </summary>
        /// <param name="indent">The number of characters to indent.</param>
        /// <param name="sb">A string builder to write the dump into.</param>
        internal void DumpSymbolTable(int indent, StringBuilder sb)
        {
            Contract.Requires(sb != null);
            lock (this.symbolTable)
            {
                var initial = new string(' ', indent);
                foreach (var ent in this.symbolTable)
                {
                    sb.AppendFormat("{0}{1}: {2}\n ", initial, ent.Name, ent.Value);
                }
            }
        }

        /// <summary>
        /// Create a list of (name . value) representing the symbol table.
        /// </summary>
        /// <returns>The name/value pairs.</returns>
        internal SchemeObject ListEnv()
        {
            SchemeObject result = EmptyList.Instance;
            lock (this.symbolTable)
            {
                foreach (var ent in this.symbolTable)
                {
                    Contract.Assume(ent.Name != null);
                    Contract.Assume(ent.Value != null);
                    result = List.Cons(List.Cons((SchemeString)ent.Name, ent.Value), result);
                }
            }

            return result;
        }

        /// <summary>
        /// Add a list of symbols and values to the symbol table.
        /// This is used when binding formals to actual parameters.
        /// If the list of symbols and vals is the same length, they are paired up.
        /// If the list of symbols is shorter, a list of the rest of the vals are bound to the last symbol.
        ///   This handles cases (2) and (3) in section 4.1.4 describing lambda formals.
        /// If the list of symbols is longer, it is an error.
        /// </summary>
        /// <param name="symbols">The list of symbols.</param>
        /// <param name="vals">The list of values.</param>
        private void AddSymbolList(SchemeObject symbols, SchemeObject vals)
        {
            Contract.Requires(symbols != null);
            Contract.Requires(vals != null);
            lock (this.symbolTable)
            {
                while (!(symbols is EmptyList))
                {
#if Check
                    if (vals is EmptyList)
                    {
                        ErrorHandlers.SemanticError("Too few actual parameters.");
                    }
#endif
                    if (symbols is Symbol)
                    {
                        // bind the symbol the the rest of the values
                        this.AddSymbolUnlocked((Symbol)symbols, vals);
                        break;
                    }

                    SchemeObject symbol = List.First(symbols);
                    if (!(symbol is Symbol))
                    {
                        ErrorHandlers.SemanticError(string.Format(@"Bad formal parameter: ""{0}""", symbol));
                    }

                    Contract.Assert(symbol is Symbol);
                    this.AddSymbolUnlocked((Symbol)symbol, List.First(vals));

                    symbols = List.Rest(symbols);
                    vals = List.Rest(vals);
                }
            }
        }

        /// <summary>
        /// Add a symbol and its value to the environment.
        /// If the symbol is not defined in the environment, add it.
        /// If the symbol is already defined in the environment, update its value.
        /// The caller is responsible for locking.
        /// </summary>
        /// <param name="symbol">The symbol name.</param>
        /// <param name="val">The value.</param>
        private void AddSymbolUnlocked(Symbol symbol, SchemeObject val)
        {
            Contract.Requires(symbol != null);
            Contract.Requires(val != null);
            var index = this.FindSymbol(symbol);
            if (index < 0)
            {
                this.symbolTable.Add(new NameValuePair(symbol.SymbolName, val));
                index = this.symbolTable.Count - 1;
            }
            else
            {
                Contract.Assert(index >= 0);
                this.symbolTable[index].Value = val;
            }

            symbol.SetLocation(index, 0);
        }

        /// <summary>
        /// Find a symbol in the symbol table.
        /// </summary>
        /// <param name="symbol">The symbol to look for.</param>
        /// <returns>The symbol position, if found, otherwise -1.</returns>
        private int FindSymbol(Symbol symbol)
        {
            Contract.Requires(symbol != null);
            Contract.Ensures(Contract.Result<int>() >= -1);
            string symbolName = symbol.SymbolName;
            for (int i = 0; i < this.symbolTable.Count; i++)
            {
                if (this.symbolTable[i].Name == symbolName)
                {
                    return i;
                }
            }

            return -1;

            // The following is slower because it creates a lambda that captures the symbolName.
            // return this.symbolTable.FindIndex(entry => entry.Name == symbolName);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// The environment is either primitive, in which case it has a symbol table only, or
        /// else it has a symbol table, lexical parent, and an associated interpreter.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.empty || this.primitive || this.interp != null);
            Contract.Invariant(this.empty || this.primitive || this.lexicalParent != null);
            Contract.Invariant(this.empty || this.symbolTable != null);
        }
        #endregion

        #region NameValuePair

        /// <summary>
        /// These make up symbol table entries.
        /// </summary>
        private class NameValuePair
        {
            /// <summary>
            /// The symbol name.
            /// </summary>
            internal readonly string Name;

            /// <summary>
            /// The symbol value.
            /// </summary>
            internal SchemeObject Value;

            /// <summary>
            /// Initializes a new instance of the NameValuePair struct.
            /// </summary>
            /// <param name="name">The symbol name.</param>
            /// <param name="value">The symbol value.</param>
            internal NameValuePair(string name, SchemeObject value)
            {
                Contract.Requires(name != null);
                Contract.Requires(value != null);
                this.Name = name;
                this.Value = value;
            }
        }
        #endregion
    }
}