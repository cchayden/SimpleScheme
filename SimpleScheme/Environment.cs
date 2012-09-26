#define OLD
// <copyright file="Environment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Diagnostics;
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
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("environment");

        /// <summary>
        /// The symbol table for this enviornment.
        /// </summary>
        private readonly ISymbolTable symbolTable;

        /// <summary>
        /// The interpreter.
        /// </summary>
        private readonly Interpreter interp;

        /// <summary>
        /// The lexically enclosing environment.
        /// </summary>
        private readonly Environment lexicalParent;
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
            this.symbolTable = SymbolTableFactory.New(0);
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
            this.symbolTable = SymbolTableFactory.New(formals, vals);
        }
        #endregion

        #region ISymbolTable
        /// <summary>
        /// The interface to the common SymbolTable interface.
        /// </summary>
        private interface ISymbolTable
        {
            /// <summary>
            /// Look up a symbol given its name.
            /// </summary>
            /// <param name="symbol">The symbol to look up.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if found in the symbol table, false if not found.</returns>
            Obj Lookup(Symbol symbol, int level);

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            void Add(Symbol symbol, Obj val);

            /// <summary>
            /// Update a value in the symbol table.
            /// </summary>
            /// <param name="symbol">The symbol to update.</param>
            /// <param name="val">The new value.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if the value was found and stored, false if not found.</returns>
            bool Update(Symbol symbol, Obj val, int level);

            /// <summary>
            /// Increment the value for a symbol.
            /// </summary>
            /// <param name="symbol">The symbol whose value should be incremented.  The existing value must be a number.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if the symbol was found (and incremented), false otherwise.</returns>
            Obj Increment(Symbol symbol, int level);

            /// <summary>
            /// Dump the symbol table to a string for printing.
            /// </summary>
            /// <param name="indent">The number of characters to indent.</param>
            /// <param name="sb">A string builder to write the dump into.</param>
            void Dump(int indent, StringBuilder sb);
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
            get { return this.lexicalParent; }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Add a new definition into the environment.
        /// If a procedure is being added, set its name.
        /// </summary>
        /// <param name="var">This is a variable to add to the environment.</param>
        /// <param name="val">This is the value of that variable.</param>
        public void Define(string var, Obj val)
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
        /// Look up a symbol in the environment.
        /// This is the single most expensive operation in the whole interpreter.
        /// Is there some way to make this faster?
        /// </summary>
        /// <param name="var">The name of the variable to look up.  Must be a symbol.</param>
        /// <returns>The value bound to the variable.</returns>
        public Obj Lookup(string var)
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

        /// <summary>
        /// Add a new definition into the environment.
        /// If a procedure is being added, set its name.
        /// </summary>
        /// <param name="var">This is a variable to add to the environment.</param>
        /// <param name="val">This is the value of that variable.</param>
        public void Define(Symbol var, Obj val)
        {
            this.symbolTable.Add(var, val);

            if (val.IsProcedure())
            {
                val.AsProcedure().SetName(var.SymbolName);
            }
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// This is the single most expensive operation in the whole interpreter.
        /// Is there some way to make this faster?
        /// </summary>
        /// <param name="var">The name of the variable to look up.  Must be a symbol.</param>
        /// <returns>The value bound to the variable.</returns>
        public Obj Lookup(Symbol var)
        {
            Environment env = this;

            // search down the chain of environments for a definition
            int level = 0;
            while (env != Empty)
            {
                Obj val = env.symbolTable.Lookup(var, level);
                if (val != null)
                {
                    return val;
                }

                // if we have not found anything yet, look in the parent
                env = env.lexicalParent;
                level++;
            }

            return ErrorHandlers.SemanticError("Unbound variable: " + var.SymbolName);
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
            if (!var.IsSymbol())
            {
                return ErrorHandlers.SemanticError("Attempt to set a non-symbol: " + Printer.AsString(var));
            }

            var symbol = var.AsSymbol();
            Environment env = this;

            // search down the chain of environments for a definition
            int level = 0;
            while (env != Empty)
            {
                if (env.symbolTable.Update(symbol, val, level))
                {
                    return val;
                }

                // if we have not found anything yet, look in the parent
                env = env.lexicalParent;
                level++;
            }

            return ErrorHandlers.SemanticError("Unbound variable in set!: " + symbol.SymbolName);
        }

        /// <summary>
        /// Increment the variable.
        /// </summary>
        /// <param name="var">The symbol naming the variable to increment.</param>
        /// <returns>The incremented value.</returns>
        public Obj Increment(Obj var)
        {
            if (!var.IsSymbol())
            {
                return ErrorHandlers.SemanticError("Attempt to increment a non-symbol: " + Printer.AsString(var));
            }

            var symbol = var.AsSymbol();
            Environment env = this;

            // search down the chain of environments for a definition
            int level = 0;
            while (env != Empty)
            {
                Obj val = env.symbolTable.Increment(symbol, level);
                if (val != null)
                {
                    return val;
                }

                // if we have not found anything yet, look in the parent
                env = env.lexicalParent;
                level++;
            }

            return ErrorHandlers.SemanticError("Unbound variable in set!: " + symbol.SymbolName);
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
            var sb = new StringBuilder();
            Environment env = this;
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

        #region SymbolTableFactory
        /// <summary>
        /// Encapsulates the policy for creating symbol tables.
        /// </summary>
        private static class SymbolTableFactory
        {
            /// <summary>
            /// Create a new symbol table with a given number of elements.
            /// </summary>
            /// <param name="count">The number of entries to start with.</param>
            /// <returns>A symbol table.</returns>
            public static ISymbolTable New(int count)
            {
                return new SymbolTableDict(count);
            }

            /// <summary>
            /// Create a symbol table and populate it with some symbol definitions.
            /// </summary>
            /// <param name="formals">A list of symbol names.</param>
            /// <param name="vals">A list of smybol values.</param>
            /// <returns>A symbol table.</returns>
            public static ISymbolTable New(Obj formals, Obj vals)
            {
                return new SymbolTableList(formals, vals);
            }
        }
        #endregion

        #region SymbolTableDict
        /// <summary>
        /// The symbol table for the environment.
        /// This implementation uses a dictionary, which is good for large symbol tables.
        /// This is always accessed within a lock, so we can use Dictionary instead of
        ///   the (presumably) more expensive ConcurrentDictionary.
        /// This is the single most expensive part of the code.
        /// Is there a way to make this faster?
        /// Most symbol tables are small, but the base environment with the primitives is large.
        /// Both of them are used very frequently.  Is there a way to look up symbols less?
        /// </summary>
        private class SymbolTableDict : ISymbolTable
        {
            /// <summary>
            /// Stores symbols and their values.
            /// </summary>
            private readonly Dictionary<string, Obj> symbolTable;

            /// <summary>
            /// Lock the symbol table before using -- may be concurrent.
            /// Alternative: use ConcurrentDictionary.
            /// </summary>
            private readonly object lockObj;

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTableDict class.
            /// </summary>
            /// <param name="count">The number of symbol table slots to pre-allocate.</param>
            public SymbolTableDict(int count)
            {
                this.symbolTable = new Dictionary<string, Obj>(count);
                this.lockObj = new object();
            }

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTableDict class and adds symbols.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            public SymbolTableDict(Obj symbols, Obj vals) : this(symbols.ListLength())
            {
                this.AddList(symbols, vals);
            }

            /// <summary>
            /// Look up a symbol given its name.
            /// </summary>
            /// <param name="symbol">The symbol to look up.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if found in the symbol table, false if not found.</returns>
            public Obj Lookup(Symbol symbol, int level)
            {
                lock (this.lockObj)
                {
                    Obj val;
                    if (this.symbolTable.TryGetValue(symbol.SymbolName, out val))
                    {
                        return val;
                    }

                    return null;
                }
            }

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            public void Add(Symbol symbol, Obj val)
            {
                lock (this.lockObj)
                {
                    this.symbolTable[symbol.SymbolName] = val;
                }
            }

            /// <summary>
            /// Update a value in the symbol table.
            /// </summary>
            /// <param name="symbol">The symbol to update.</param>
            /// <param name="val">The new value.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if the value was found and stored, false if not found.</returns>
            public bool Update(Symbol symbol, Obj val, int level)
            {
                lock (this.lockObj)
                {
                    if (!this.symbolTable.ContainsKey(symbol.SymbolName))
                    {
                        return false;
                    }

                    this.symbolTable[symbol.SymbolName] = val;
                    return true;
                }
            }

            /// <summary>
            /// Increment the value for a symbol.
            /// </summary>
            /// <param name="symbol">The symbol whose value should be incremented.  The existing value must be a number.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if the symbol was found (and incremented), false otherwise.</returns>
            public Obj Increment(Symbol symbol, int level)
            {
                lock (this.lockObj)
                {
                    Obj val;
                    if (!this.symbolTable.TryGetValue(symbol.SymbolName, out val))
                    {
                        // If not found, it is not an error, just keep looking.
                        return null;
                    }

                    if (!val.IsNumber())
                    {
                        // If it is found but is not a number, then that is an error.
                        ErrorHandlers.SemanticError("Attempt to increment a non-number: " + Printer.AsString(val));
                        return null;
                    }

                    this.symbolTable[symbol.SymbolName] = val.AsNumber() + 1;
                    return val;
                }
            }

            /// <summary>
            /// Dump the symbol table to a string for printing.
            /// </summary>
            /// <param name="indent">The number of characters to indent.</param>
            /// <param name="sb">A string builder to write the dump into.</param>
            public void Dump(int indent, StringBuilder sb)
            {
                lock (this.lockObj)
                {
                    var initial = new string(' ', indent);
                    foreach (var key in this.symbolTable.Keys)
                    {
                        sb.AppendFormat("{0}{1}: {2}\n", initial, key, this.symbolTable[key]);
                    }
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
                while (!symbols.IsEmptyList())
                {
                    if (symbols.IsSymbol())
                    {
                        this.Add(symbols.AsSymbol(), vals);
                    }
                    else
                    {
                        Obj symbol = symbols.First();
                        if (!symbol.IsSymbol())
                        {
                            ErrorHandlers.SemanticError("Bad formal parameter: " + symbol);
                        }

                        this.Add(symbol.AsSymbol(), vals.First());
                    }

                    symbols = symbols.Rest();
                    vals = vals.Rest();
                }
            }
        }
        #endregion

        #region SymbolTableList
        /// <summary>
        /// A SymbolTable based on List.  This is inefficient for large symbol tables,
        /// but if the caller can remember the position, it can be more efficient.
        /// </summary>
        private class SymbolTableList : ISymbolTable
        {
            /// <summary>
            /// Stores the symbols.
            /// </summary>
            private readonly List<string> names;

            /// <summary>
            /// The values.
            /// </summary>
            private readonly List<object> values;

            /// <summary>
            /// Lock the symbol table before using -- may be concurrent.
            /// Alternative: use ConcurrentDictionary.
            /// </summary>
            private readonly object lockObj;

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTableList class and adds symbols.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            public SymbolTableList(Obj symbols, Obj vals) : this(symbols.ListLength())
            {
                this.AddList(symbols, vals);
            }

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTableList class.
            /// </summary>
            /// <param name="count">The number of symbol table slots to pre-allocate.</param>
            private SymbolTableList(int count)
            {
                this.names = new List<string>(count);
                this.values = new List<object>(count);
                this.lockObj = new object();
            }

            /// <summary>
            /// Look up a symbol given its name.
            /// If the symbol is not found at this level, then return null and we will continue to search
            /// through the parent environment chain.
            /// </summary>
            /// <param name="symbol">The symbol to look up.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if found in the symbol table, false if not found.</returns>
            public Obj Lookup(Symbol symbol, int level)
            {
                lock (this.lockObj)
                {
                    var index = this.names.FindIndex(name => name == symbol.SymbolName);
                    if (index == -1)
                    {
                        return null;
                    }

                    return this.values[index];
                }
            }

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// If the symbol is not defined in the environment, add it.
            /// If the symbol is already defined in the envhronment, update its value.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            public void Add(Symbol symbol, Obj val)
            {
                lock (this.lockObj)
                {
                    var index = this.names.FindIndex(name => name == symbol.SymbolName);
                    if (index == -1)
                    {
                        this.names.Add(symbol.SymbolName);
                        this.values.Add(val);
                        return;
                    }

                    this.values[index] = val;
                }
            }

            /// <summary>
            /// Update a value in the symbol table.
            /// If the symbol is not defined, return false (this is an error).
            /// </summary>
            /// <param name="symbol">The symbol to update.</param>
            /// <param name="val">The new value.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if the value was found and stored, false if not found.</returns>
            public bool Update(Symbol symbol, Obj val, int level)
            {
                lock (this.lockObj)
                {
                    var index = this.names.FindIndex(name => name == symbol.SymbolName);
                    if (index == -1)
                    {
                        return false;
                    }

                    this.values[index] = val;
                    return true;
                }
            }

            /// <summary>
            /// Increment the value for a symbol.
            /// </summary>
            /// <param name="symbol">The symbol whose value should be incremented.  The existing value must be a number.</param>
            /// <param name="level">The number of levels from the use to the definition.</param>
            /// <returns>True if the symbol was found (and incremented), false otherwise.</returns>
            public Obj Increment(Symbol symbol, int level)
            {
                lock (this.lockObj)
                {
                    var index = this.names.FindIndex(name => name == symbol.SymbolName);
                    if (index == -1)
                    {
                        return null;
                    }

                    Obj val = this.values[index];
                    if (!val.IsNumber())
                    {
                        // If it is found but is not a number, then that is an error.
                        ErrorHandlers.SemanticError("Attempt to increment a non-number: " + Printer.AsString(val));
                        return null;
                    }

                    this.values[index] = val.AsNumber() + 1;
                    return val;
                }
            }

            /// <summary>
            /// Dump the symbol table to a string for printing.
            /// </summary>
            /// <param name="indent">The number of characters to indent.</param>
            /// <param name="sb">A string builder to write the dump into.</param>
            public void Dump(int indent, StringBuilder sb)
            {
                lock (this.lockObj)
                {
                    var initial = new string(' ', indent);
                    for (int i = 0; i < this.names.Count; i++)
                    {
                        sb.AppendFormat("{0}{1}: {2}\n", initial, this.names[i], this.values[i]);
                    }
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
                while (!symbols.IsEmptyList())
                {
                    if (symbols.IsSymbol())
                    {
                        this.Add(symbols.AsSymbol(), vals);
                    }
                    else
                    {
                        Obj symbol = symbols.First();
                        if (!symbol.IsSymbol())
                        {
                            ErrorHandlers.SemanticError("Bad formal parameter: " + symbol);
                        }

                        this.Add(symbol.AsSymbol(), vals.First());
                    }

                    symbols = symbols.Rest();
                    vals = vals.Rest();
                }
            }
        }
        #endregion
    }
}