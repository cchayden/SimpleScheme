// <copyright file="Environment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Concurrent;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents the interpreter environment.
    /// The environment is part of a chain, made up of the Caller links, that represents the dynamic context.
    /// At the bottom of the chain is the global environment, created by the single-arg constructor.
    /// Each link in the chain contains a symbol table of the bindings at that level.
    /// Each lookup searches down the symbol tables in the chain, from the top to the bottom.
    /// </summary>
    public class Environment : IEnvironment
    {
        #region Constants
        /// <summary>
        /// Represents the end of the environment chain.
        /// </summary>
        protected const Environment Empty = null;

        /// <summary>
        /// Used for an interpreter with no environment (the primitive environment).
        /// </summary>
        protected const Interpreter NullInterp = null;
        #endregion

        #region Fields
        /// <summary>
        /// The symbol table for this enviornment.
        /// </summary>
        private readonly SymbolTable symbolTable;

        /// <summary>
        /// The interpreter.
        /// </summary>
        private readonly Interpreter interp;

        /// <summary>
        /// The lexically enclosing environment.
        /// </summary>
        private readonly Environment lexicalParent;

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("environment");
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// This is used to create the global environment.
        /// When we refer to "parent" we mean the enclosing lexical environment.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="lexicalParent">The lexical parent environment.</param>
        public Environment(Interpreter interp, Environment lexicalParent)
        {
            this.interp = interp;
            this.lexicalParent = lexicalParent;
            this.symbolTable = new SymbolTable(0);
            if (interp != NullInterp)
            {
                interp.IncrementCounter(counter);
            }
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// Creates a new empty environment.
        /// </summary>
        /// <param name="lexicalParent">The lexically enclosing environment.</param>
        /// <returns>The new environment.</returns>
        public Environment(Environment lexicalParent) : 
            this(lexicalParent.Interp, lexicalParent)
        {
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// Start out with a set of variable bindings and a lexical parent environment.
        /// The initial variable bindings are the formal parameters and the corresponding argument values.
        /// </summary>
        /// <param name="formals">A list of variable names.</param>
        /// <param name="vals">The values for these variables.</param>
        /// <param name="lexicalParent">The lexical parent environment.</param>
        public Environment(Obj formals, Obj vals, Environment lexicalParent)
        {
            this.interp = lexicalParent.Interp;
            this.lexicalParent = lexicalParent;
            this.symbolTable = new SymbolTable(formals, vals);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the interpreter used to hold the global context.
        /// The bottom environment holds the interpreter, but each one copies from its
        ///   lexical parent, so that it can be accessed directly.
        /// This field is written only in the constructor -- it is never modified.
        /// </summary>
        public Interpreter Interp
        {
            get { return this.interp; }
        }

        /// <summary>
        /// Gets the lexical parent environment.
        /// </summary>
        public Environment LexicalParent
        {
            get { return lexicalParent; }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Add a new definition into the environment.
        /// If a procedure is being added, set its name.
        /// </summary>
        /// <param name="var">This is a variable to add to the environment.</param>
        /// <param name="val">This is the value of that variable.</param>
        public void Define(Obj var, Obj val)
        {
            try
            {
               this.UnsafeDefine(var, val); 
            }
            catch (ErrorHandlers.SchemeException)
            {
            }
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// This is the single most expensive operation in the whole interpreter.
        /// Is there some way to make this faster?
        /// </summary>
        /// <param name="var">The name of the variable to look up.  Must be a symbol.</param>
        /// <returns>The value bound to the variable.</returns>
        public Obj Lookup(Obj var)
        {
            try
            {
               return this.UnsafeLookup(var); 
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

        /// <summary>
        /// Add a new definition into the environment.
        /// If a procedure is being added, set its name.
        /// </summary>
        /// <param name="var">This is a variable to add to the environment.</param>
        /// <param name="val">This is the value of that variable.</param>
        public void UnsafeDefine(Obj var, Obj val)
        {
            if (Symbol.Is(var))
            {
                string symbol = Symbol.As(var);
                lock (this)
                {
                    this.symbolTable.Add(symbol, val);

                    if (Procedure.Is(val))
                    {
                        Procedure.As(val).SetName(var.ToString());
                    }
                }

                return;
            }

            ErrorHandlers.SemanticError("Bad variable in define: " + var);
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// This is the single most expensive operation in the whole interpreter.
        /// Is there some way to make this faster?
        /// </summary>
        /// <param name="var">The name of the variable to look up.  Must be a symbol.</param>
        /// <returns>The value bound to the variable.</returns>
        public Obj UnsafeLookup(Obj var)
        {
            string symbol = Symbol.As(var);
            Environment env = this;

            // search down the chain of environments for a definition
            lock (this)
            {
                while (env != Empty)
                {
                    Obj val;
                    if (env.symbolTable.Lookup(symbol, out val))
                    {
                        return val;
                    }

                    // if we have not found anything yet, look in the parent
                    env = env.lexicalParent;
                }
            }

            return ErrorHandlers.SemanticError("Unbound variable: " + symbol);
        }

        /// <summary>
        /// Set the value of a variable in the environment to a new value.
        /// Var must be a string, the variable name.
        /// Searches the symbol table first.
        /// If not found, then searches the parent to find the value.
        /// Then set it to the new value.
        /// </summary>
        /// <param name="var">The variable name.</param>
        /// <param name="val">The new value for the variable.</param>
        /// <returns>The value that the variable was set to.</returns>
        public Obj Set(Obj var, Obj val)
        {
            if (!Symbol.Is(var))
            {
                return ErrorHandlers.SemanticError("Attempt to set a non-symbol: " + Printer.AsString(var));
            }

            string symbol = Symbol.As(var);
            Environment env = this;

            // search down the chain of environments for a definition
            lock (this)
            {
                while (env != Empty)
                {
                    if (env.symbolTable.Update(symbol, val))
                    {
                        return val;
                    }

                    // if we have not found anything yet, look in the parent
                    env = env.lexicalParent;
                }
            }

            return ErrorHandlers.SemanticError("Unbound variable in set!: " + symbol);
        }

        /// <summary>
        /// Increment the variable.
        /// This should be atomic.
        /// TODO is this atomic?
        /// </summary>
        /// <param name="var">The symbol naming the variable to increment.</param>
        /// <returns>The incremented value.</returns>
        public Obj Increment(Obj var)
        {
            if (!Symbol.Is(var))
            {
                return ErrorHandlers.SemanticError("Attempt to increment a non-symbol: " + Printer.AsString(var));
            }

            string symbol = Symbol.As(var);
            Environment env = this;

            // search down the chain of environments for a definition
            lock (this)
            {
                while (env != Empty)
                {
                    Obj val;
                    if (env.symbolTable.Increment(symbol, out val))
                    {
                        return val;
                    }

                    // if we have not found anything yet, look in the parent
                    env = env.lexicalParent;
                }
            }

            return ErrorHandlers.SemanticError("Unbound variable in set!: " + symbol);
        }

        /// <summary>
        /// Dump the stack of environments.
        /// At each level, show the symbol table.
        /// </summary>
        /// <param name="levels">The number of levels to show.</param>
        /// <param name="indent">The number of characters to indent.</param>
        /// <returns>The environment stack, as a string.</returns>
        public string Dump(int levels, int indent)
        {
            StringBuilder sb = new StringBuilder();
            Environment env = this;
            lock (this)
            {
                while (env != Empty && levels > 0)
                {
                    env.symbolTable.Dump(indent, sb);
                    levels--;
                    if (levels > 0)
                    {
                        sb.Append("-----\n");
                    }

                    env = env.lexicalParent;
                }
            }

            return sb.ToString();
        }

        /// <summary>
        /// Dump the environment.
        /// </summary>
        public void DumpEnv()
        {
            this.interp.CurrentOutputPort.WriteLine(this.Dump(100, 0));
        }

        /// <summary>
        /// Dump the top level environment.
        /// </summary>
        /// <returns>The environment, as a string.</returns>
        public string Dump()
        {
            return this.Dump(1, 0);
        }
        #endregion

        #region Private Class
        /// <summary>
        /// The symbol table for the environment.
        /// This is the single most expensive part of the code.
        /// Is there a way to make this faster?
        /// Most symbol tables are small, but the base environment with the primitives is large.
        /// Both of them are used very frequently.  Is there a way to look up symbols less?
        /// </summary>
        private class SymbolTable
        {
            /// <summary>
            /// Stores symbols and their values.
            /// </summary>
            private readonly ConcurrentDictionary<string, Obj> symbolTable;

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTable class.
            /// </summary>
            /// <param name="count">The number of symbol table slots to pre-allocate.</param>
            public SymbolTable(int count)
            {
                this.symbolTable = new ConcurrentDictionary<string, Obj>(1, count);
            }

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTable class and adds symbols.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            public SymbolTable(Obj symbols, Obj vals)
            {
                this.symbolTable = new ConcurrentDictionary<string, Obj>(1, List.Length(symbols));
                this.AddList(symbols, vals);
            }

            /// <summary>
            /// Look up a symbol given its name.
            /// </summary>
            /// <param name="symbol">The symbol to look up.</param>
            /// <param name="val">Its returned value.</param>
            /// <returns>True if found in the symbol table, false if not found.</returns>
            public bool Lookup(string symbol, out Obj val)
            {
                return this.symbolTable.TryGetValue(symbol, out val);
            }

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            public void Add(string symbol, Obj val)
            {
                this.symbolTable.AddOrUpdate(symbol, val, (key, existingValue) => val);
            }

            /// <summary>
            /// Update a value in the symbol table.
            /// This is not really thread safe -- there is no operation on the symbol table to update only
            ///   if found.  But since items are never removed, some concurrent action that defines the symbol
            ///   can have its value overwritten.
            /// </summary>
            /// <param name="symbol">The symbol to update.</param>
            /// <param name="val">The new value.</param>
            /// <returns>True if the value was found and stored, false if not found.</returns>
            public bool Update(string symbol, Obj val)
            {
                if (!this.symbolTable.ContainsKey(symbol))
                {
                    return false;
                }

                this.symbolTable[symbol] = val;
                return true;
            }

            /// <summary>
            /// Increment the value for a symbol.
            /// This is not really thread safe, since there is no operation on symbolTable to update only
            ///   if the value is present.  Since items are never removed, AddOrUpdate can never add, only update.
            /// </summary>
            /// <param name="symbol">The symbol whose value should be incremented.  The existing value must be a number.</param>
            /// <param name="val">The new value.</param>
            /// <returns>True if the symbol was found (and incremented), false otherwise.</returns>
            public bool Increment(string symbol, out Obj val)
            {
                if (!this.symbolTable.TryGetValue(symbol, out val))
                {
                    return false;
                }

                val = this.symbolTable.AddOrUpdate(
                    symbol,
                    0,
                    (key, existingValue) =>
                        {
                            if (!Number.Is(existingValue))
                            {
                                ErrorHandlers.SemanticError("Attempt to increment a non-number: " + Printer.AsString(existingValue));
                                return 0;
                            }

                            return Number.As(existingValue) + 1;
                        });
                return true;
            }

            /// <summary>
            /// Dump the symbol table to a string for printing.
            /// </summary>
            /// <param name="indent">The number of characters to indent.</param>
            /// <param name="sb">A string builder to write the dump into.</param>
            public void Dump(int indent, StringBuilder sb)
            {
                string initial = new string(' ', indent);
                foreach (var key in this.symbolTable.Keys)
                {
                    sb.AppendFormat("{0}{1}: {2}\n", initial, key, this.symbolTable[key]);
                }
            }

            /// <summary>
            /// Add a list of symbols and values.
            /// The list of symbols and their values must be the same length.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            private void AddList(Obj symbols, Obj vals)
            {
                while (!EmptyList.Is(symbols))
                {
                    if (Symbol.Is(symbols))
                    {
                        this.Add(Symbol.As(symbols), vals);
                    }
                    else
                    {
                        Obj symbol = List.First(symbols);
                        if (!Symbol.Is(symbol))
                        {
                            ErrorHandlers.SemanticError("Bad formal parameter: " + symbol);
                        }

                        this.Add(Symbol.As(symbol), List.First(vals));
                    }

                    symbols = List.Rest(symbols);
                    vals = List.Rest(vals);
                }
            }
        }
        #endregion
    }
}