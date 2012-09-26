// <copyright file="Environment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Diagnostics;
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
        #region Constants
        /// <summary>
        /// Represents the end of the environment chain.
        /// </summary>
        private static readonly Environment emptyEnvironment = null;

        /// <summary>
        /// Used for an interpreter with no environment (the primitive environment).
        /// </summary>
        private static readonly Interpreter nullInterp = null;
        #endregion

        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("environment");

        /// <summary>
        /// The interpreter.
        /// </summary>
        private readonly Interpreter interp;

        /// <summary>
        /// The lexically enclosing environment.
        /// </summary>
        private readonly Environment lexicalParent;

        /// <summary>
        /// The symbol table for this enviornment.
        /// </summary>
        private SymbolTable symbolTable;
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
            if (interp != nullInterp)
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
        public Environment(SchemeObject formals, SchemeObject vals, Environment lexicalParent)
        {
            this.interp = lexicalParent.Interp;
            this.lexicalParent = lexicalParent;
            this.symbolTable = new SymbolTable(formals, vals);
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// This is used to create the base of the environment chain.
        /// </summary>
        public Environment() : this(nullInterp, emptyEnvironment)
        {
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
            get { return this.interp; }
        }

        /// <summary>
        /// Gets the lexical parent environment.
        /// </summary>
        internal Environment LexicalParent
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
               this.Define(Symbol.New(var), val);
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
               this.Set(Symbol.New(var), val);
            }
            catch (ErrorHandlers.SchemeException)
            {
            }
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// </summary>
        /// <param name="var">The name of the variable to look up.  Must name a symbol.</param>
        /// <returns>The value bound to the variable.</returns>
        public SchemeObject Lookup(string var)
        {
            try
            {
               return this.Lookup(Symbol.New(var));
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
            this.symbolTable.Add(var, val);

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
            Environment env = this;

            // search down the chain of environments for a definition
            int level = 0;
            if (var.Located)
            {
                while (level < var.Level)
                {
                    env = env.LexicalParent;
                    level++;
                }

                return env.symbolTable.Lookup(var, level);
            }

            while (env != emptyEnvironment)
            {
                SchemeObject val = env.symbolTable.Lookup(var, level);
                if (val != null)
                {
                    return val;
                }

                // if we have not found anything yet, look in the parent
                env = env.lexicalParent;
                level++;
            }

            return ErrorHandlers.SemanticError(string.Format(@"Unbound variable: ""{0}""", var.SymbolName), var);
        }

        /// <summary>
        /// Add or update a symbol table entry at this level.
        /// </summary>
        /// <param name="symbol">The symbol to update.</param>
        /// <param name="val">The new value.</param>
        internal void Update(SchemeObject symbol, SchemeObject val)
        {
            if (!(symbol is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Attempt to update a non-symbol: ""{0}""", symbol.ToString(true)), symbol);
            }

            this.symbolTable.Add((Symbol)symbol, val);
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
            Environment env = this;

            // search down the chain of environments for a definition
            int level = 0;
            if (symbol.Located)
            {
                while (level < symbol.Level)
                {
                    env = env.LexicalParent;
                    level++;
                }

                env.symbolTable.Update(symbol, val, level);
                return;
            }

            while (env != emptyEnvironment)
            {
                if (env.symbolTable.Update(symbol, val, level) != null)
                {
                    return;
                }

                // if we have not found anything yet, look in the parent
                env = env.lexicalParent;
                level++;
            }

            ErrorHandlers.SemanticError(string.Format(@"Unbound variable in set!: ""{0}""", symbol.SymbolName), symbol);
        }

        /// <summary>
        /// Increment the variable.
        /// </summary>
        /// <param name="var">The symbol naming the variable to increment.</param>
        /// <returns>The incremented value.</returns>
        internal SchemeObject Increment(SchemeObject var)
        {
            if (!(var is Symbol))
            {
                return ErrorHandlers.SemanticError(string.Format(@"Attempt to increment a non-symbol: ""{0}""", var.ToString(true)), var);
            }

            var symbol = (Symbol)var;
            Environment env = this;

            // search down the chain of environments for a definition
            while (env != emptyEnvironment)
            {
                SchemeObject val = env.symbolTable.Increment(symbol);
                if (val != null)
                {
                    return val;
                }

                // if we have not found anything yet, look in the parent
                env = env.lexicalParent;
            }

            return ErrorHandlers.SemanticError(string.Format(@"Unbound variable in set!: ""{0}""", symbol.SymbolName), symbol);
        }

        /// <summary>
        /// Dump the stack of environments.
        /// At each level, show the symbol table.
        /// </summary>
        /// <param name="levels">The number of levels to show.</param>
        /// <param name="indent">The number of characters to indent.</param>
        /// <returns>The environment stack, as a string.</returns>
        internal string Dump(int levels, int indent)
        {
            var sb = new StringBuilder();
            Environment env = this;
            while (env != emptyEnvironment && levels > 0)
            {
                env.symbolTable.Dump(indent, sb);
                levels--;
                if (levels > 0)
                {
                    sb.Append("-----\n ");
                }

                env = env.lexicalParent;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Dump the environment.
        /// </summary>
        internal void DumpEnv()
        {
            this.interp.CurrentOutputPort.WriteLine(this.Dump(100, 0));
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
        /// Make a list of all the values in the environment.
        /// </summary>
        /// <returns>The list of (name . value) pairs.</returns>
        internal SchemeObject ListEnv()
        {
            return this.symbolTable.ListEnv();
        }
        #endregion

        #region NameValuePair
        /// <summary>
        /// These make up symbol table entries.
        /// </summary>
        private struct NameValuePair
        {
            /// <summary>
            /// The symbol name.
            /// </summary>
            internal readonly string Name;

            /// <summary>
            /// The symbol value.
            /// </summary>
            internal readonly SchemeObject Value;

            /// <summary>
            /// Initializes a new instance of the NameValuePair struct.
            /// </summary>
            /// <param name="name">The symbol name.</param>
            /// <param name="value">The symbol value.</param>
            internal NameValuePair(string name, SchemeObject value)
            {
                this.Name = name;
                this.Value = value;
            }
        }
        #endregion

        #region SymbolTable
        /// <summary>
        /// A SymbolTable based on List.  This is inefficient for large symbol tables,
        /// but if the caller can remember the position, it can be more efficient.
        /// </summary>
        private struct SymbolTable
        {
            /// <summary>
            /// Stores the name/value pairs.
            /// </summary>
            private readonly List<NameValuePair> entries;

            /// <summary>
            /// Lock the symbol table before using -- may be concurrent.
            /// Alternative: use ConcurrentDictionary.
            /// </summary>
            private readonly object lockObj;

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTable struct and adds symbols.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            internal SymbolTable(SchemeObject symbols, SchemeObject vals) : this(List.ListLength(symbols))
            {
                this.AddList(symbols, vals);
            }

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTable struct.
            /// </summary>
            /// <param name="count">The number of symbol table slots to pre-allocate.</param>
            internal SymbolTable(int count)
            {
                this.entries = new List<NameValuePair>(count);
                this.lockObj = new object();
            }

            /// <summary>
            /// Look up a symbol given its name.
            /// If the symbol is not found at this level, then return null and we will continue to search
            /// through the parent environment chain.
            /// </summary>
            /// <param name="symbol">The symbol to look up.</param>
            /// <param name="level">The number of levels to travel to find the definition.  Cached in the symbol the
            ///   first time it is looked up.</param>
            /// <returns>The value of the object looked up, null if not found.</returns>
            internal SchemeObject Lookup(Symbol symbol, int level)
            {
                lock (this.lockObj)
                {
                    if (symbol.Located)
                    {
                        Debug.Assert(level == symbol.Level, "SymbolTable:Lookup level -- symbol.Level");
                        return this.entries[symbol.Pos].Value;
                    }

                    var index = this.FindSymbol(symbol);
                    if (!SymbolFound(index))
                    {
                        return null;
                    }

                    symbol.SetLocation(index, level);
                    return this.entries[index].Value;
                }
            }

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// If the symbol is not defined in the environment, add it.
            /// If the symbol is already defined in the envhronment, update its value.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            internal void Add(Symbol symbol, SchemeObject val)
            {
                lock (this.lockObj)
                {
                    this.AddUnlocked(symbol, val);
                }
            }

            /// <summary>
            /// Update a value in the symbol table.
            /// If the symbol is not defined, return false.
            /// Cache the symbol location if not already done.
            /// </summary>
            /// <param name="symbol">The symbol to update.</param>
            /// <param name="val">The new value.</param>
            /// <param name="level">The number of levels to travel to find the definition</param>
            /// <returns>The new value, null if not found.</returns>
            internal SchemeObject Update(Symbol symbol, SchemeObject val, int level)
            {
                var entry = new NameValuePair(symbol.SymbolName, val);
                lock (this.lockObj)
                {
                    if (symbol.Located)
                    {
                        if (level != symbol.Level)
                        {
                            return null;
                        }

                        this.entries[symbol.Pos] = entry;
                        return val;
                    }

                    var index = this.FindSymbol(symbol);
                    if (!SymbolFound(index))
                    {
                        return null;
                    }

                    this.entries[index] = entry;
                    symbol.SetLocation(index, level);
                    return val;
                }
            }

            /// <summary>
            /// Increment the value for a symbol at this level.
            /// </summary>
            /// <param name="symbol">The symbol whose value should be incremented.  The existing value must be a number.</param>
            /// <returns>The new value, null if not found.</returns>
            internal SchemeObject Increment(Symbol symbol)
            {
                lock (this.lockObj)
                {
                    var index = this.FindSymbol(symbol);
                    if (!SymbolFound(index))
                    {
                        return null;
                    }

                    SchemeObject val = this.entries[index].Value;
                    if (!(val is Number))
                    {
                        // If it is found but is not a number, then that is an error.
                        ErrorHandlers.SemanticError(string.Format(@"Attempt to increment a non-number: ""{0}""", val.ToString(true)), symbol);
                        return null;
                    }

                    this.entries[index] = new NameValuePair(symbol.SymbolName, (Number)(((Number)val).N + 1));
                    return val;
                }
            }

            /// <summary>
            /// Dump the symbol table to a string for printing.
            /// </summary>
            /// <param name="indent">The number of characters to indent.</param>
            /// <param name="sb">A string builder to write the dump into.</param>
            internal void Dump(int indent, StringBuilder sb)
            {
                lock (this.lockObj)
                {
                    var initial = new string(' ', indent);
                    foreach (var ent in this.entries)
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
                lock (this.lockObj)
                {
                    foreach (var ent in this.entries)
                    {
                        result = List.Cons(List.Cons((SchemeString)ent.Name, ent.Value), result);
                    }
                }

                return result;
            }

            /// <summary>
            /// Tests if the symbol was found.
            /// </summary>
            /// <param name="index">The return value from FindSymbol.</param>
            /// <returns>True if a symbol was found.</returns>
            private static bool SymbolFound(int index)
            {
                return index != -1;
            }

            /// <summary>
            /// Find a symbol in the symbol table.
            /// </summary>
            /// <param name="symbol">The symbol to look for.</param>
            /// <returns>The symbol position, if found, otherwise -1.</returns>
            private int FindSymbol(Symbol symbol)
            {
                string symbolName = symbol.SymbolName;
                for (int i = 0; i < this.entries.Count; i++)
                {
                    if (this.entries[i].Name == symbolName)
                    {
                        return i;
                    }
                }

                return -1;
//              The following is slower because it creates a lambda that captures the symbolName.
//              This is in the critical path, so use the more verbose choice.
//              return this.entries.FindIndex(entry => entry.Name == symbolName);
            }

            /// <summary>
            /// Add a list of symbols and values.
            /// If the list of symbols and vals is the same length, they are paired up.
            /// If the list of symbols is shorter, a list of the rest of the vals are bound to the last symbol.
            ///   This handles cases (2) and (3) in section 4.1.4 describing lambda formals.
            /// If the list of symbols is longer, it is an error.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            private void AddList(SchemeObject symbols, SchemeObject vals)
            {
                lock (this.lockObj)
                {
                    while (!(symbols is EmptyList))
                    {
                        if (symbols is Symbol)
                        {
                            // bind the symbol the the rest of the values
                            this.AddUnlocked((Symbol)symbols, vals);
                        }
                        else
                        {
                            SchemeObject symbol = List.First(symbols);
                            if (!(symbol is Symbol))
                            {
                                ErrorHandlers.SemanticError(
                                    string.Format(@"Bad formal parameter: ""{0}""", symbol), symbol);
                            }

                            this.AddUnlocked((Symbol)symbol, List.First(vals));
                        }

                        symbols = List.Rest(symbols);
                        vals = List.Rest(vals);
                    }
                }
            }

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// If the symbol is not defined in the environment, add it.
            /// If the symbol is already defined in the envhronment, update its value.
            /// The caller is responsible for locking.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            private void AddUnlocked(Symbol symbol, SchemeObject val)
            {
                var index = this.FindSymbol(symbol);
                if (!SymbolFound(index))
                {
                    this.entries.Add(new NameValuePair(symbol.SymbolName, val));
                    index = this.entries.Count - 1;
                }
                else
                {
                    this.entries[index] = new NameValuePair(symbol.SymbolName, val);
                }

                symbol.SetLocation(index, 0);
            }
        }
        #endregion
    }
}