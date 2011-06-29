// <copyright file="Environment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents the interpreter environment.
    /// The environment is part of a chain, made up of the Caller links, that represents the dynamic context.
    /// At the bottom of the chain is the global environment, created by the single-arg constructor.
    /// Each link in the chain contains a symbol table of the bindings at that level.
    /// Each lookup searches down the symbol tables in the chain, from the top to the bottom.
    /// </summary>
    public sealed class Environment : ListPrimitives, IEnvironment
    {
        #region Fields
        /// <summary>
        /// Represents the end of the environment chain.
        /// </summary>
        private const Environment Empty = null;

        /// <summary>
        /// The symbol table for this enviornment.
        /// </summary>
        private readonly SymbolTable symbolTable;

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
        private Environment(Interpreter interp, Environment lexicalParent)
        {
            this.Interp = interp;
            this.LexicalParent = lexicalParent;
            this.symbolTable = new SymbolTable(0);
            if (interp != null)
            {
                interp.IncrementCounter(counter);
            }
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// Start out with a set of variable bindings and a lexical parent environment.
        /// The initial variable bindings are the formal parameters and the corresponding argument values.
        /// </summary>
        /// <param name="formals">A list of variable names.</param>
        /// <param name="vals">The values for these variables.</param>
        /// <param name="lexicalParent">The lexical parent environment.</param>
        private Environment(Obj formals, Obj vals, Environment lexicalParent)
        {
            this.LexicalParent = lexicalParent;
            this.Interp = lexicalParent.Interp;
            int count;
            if (!CheckArgCount(formals, vals, out count))
            {
                ErrorHandlers.Warn("Wrong number of arguments: expected " + formals + " got " + vals);
            }

            this.symbolTable = new SymbolTable(count);
            this.symbolTable.AddList(formals, vals);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the interpreter used to hold the global context.
        /// The bottom environment holds the interpreter, but each one copies from its
        ///   lexical parent, so that it can be accessed directly.
        /// This field is written only in the constructor -- it is never modified.
        /// </summary>
        internal Interpreter Interp { get; private set; }

        /// <summary>
        /// Gets the lexical parent environment.
        /// </summary>
        internal Environment LexicalParent { get; private set; }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Create a new primitive environment.
        /// This is used to share the primitives among several interpreters.
        /// </summary>
        /// <returns>The primitive environment.</returns>
        public static Environment NewPrimitive()
        {
            return new Environment(null, null);
        }

        /// <summary>
        /// Install primitives into the environment.
        /// </summary>
        public void InstallPrimitives()
        {
            Environment env = this;
            EvaluateExpression.DefinePrimitives(env);
            Number.DefinePrimitives(env);
            Procedure.DefinePrimitives(env);
            List.DefinePrimitives(env);
            InputPort.DefinePrimitives(env);
            OutputPort.DefinePrimitives(env);
            Vector.DefinePrimitives(env);
            SchemeBoolean.DefinePrimitives(env);
            SchemeString.DefinePrimitives(env);
            Character.DefinePrimitives(env);
            Symbol.DefinePrimitives(env);
            ClrProcedure.DefinePrimitives(env);
            SynchronousClrProcedure.DefinePrimitives(env);
            AsynchronousClrProcedure.DefinePrimitives(env);
            Counter.DefinePrimitives(env);
            Interpreter.DefinePrimitives(env);
            ErrorHandlers.DefinePrimitives(env);

            env
                .DefinePrimitive(
                   "exit", 
                   (args, caller) =>
                       {
                            System.Environment.Exit(First(args) == List.Empty ? 0 : (int)Number.Num(First(args)));
                            return Undefined.Instance;
                        },
                   0, 
                   1)
                .DefinePrimitive("time-call", (args, caller) => EvaluateTimeCall.Call(args, caller.Env, caller), 1, 2);
        }
        #endregion

        #region Public Methods
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
        /// <returns>The variable added to the environment.</returns>
        public Obj Define(Obj var, Obj val)
        {
            if (var is string)
            {
                this.symbolTable.Add((string)var, val);

                if (val is Procedure)
                {
                    ((Procedure)val).SetName(var.ToString());
                }

                return var;
            }

            return ErrorHandlers.Error("Bad variable in define: " + var);
        }

        /// <summary>
        /// Define a primitive, taking a variable number of arguments.
        /// Creates a Primitive and puts it in the environment associated 
        ///    with the given name.
        /// Returns the environmet interface.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public IEnvironment DefinePrim(string name, Op operation, int minArgs, int maxArgs)
        {
            return this.DefinePrimitive(name, operation, minArgs, maxArgs);
        }

        /// <summary>
        /// Define a primitive, taking a fixed number of arguments.
        /// Creates a Primitive and puts it in the environment associated 
        ///    with the given name.
        /// Returns the environmet interface.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="numberOfArgs">The number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public IEnvironment DefinePrim(string name, Op operation, int numberOfArgs)
        {
            return this.DefinePrimitive(name, operation, numberOfArgs);
        }

        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Creates a new Environment.
        /// This is used to create the global environment.
        /// When we refer to "parent" we mean the enclosing lexical environment.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent environment.</param>
        /// <returns>The global environment.</returns>
        internal static Environment NewGlobal(Interpreter interp, Environment parent)
        {
            return new Environment(interp, parent);
        }

        /// <summary>
        /// Create a new Environment.
        /// This is for where there is a new binding context.
        /// Start out with a set of variable bindings and a parent environment.
        /// The initial variable bindings are the formal parameters and the corresponding argument values.
        /// </summary>
        /// <param name="formals">A list of variable names.</param>
        /// <param name="vals">The values for these variables.</param>
        /// <param name="parent">The parent environment.</param>
        /// <returns>The new environment.</returns>
        internal static Environment New(Obj formals, Obj vals, Environment parent)
        {
            return new Environment(formals, vals, parent);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Look up a symbol in the environment.
        /// </summary>
        /// <param name="symbol">The name of the variable to look up.</param>
        /// <returns>The value bound to the variable.</returns>
        internal Obj Lookup(string symbol)
        {
            Environment env = this;
            while (env != Empty)
            {
                Obj val;
                if (env.symbolTable.Lookup(symbol, out val))
                {
                    return val;
                }

                // if we get here, we have not found anything, so look in the parent
                env = env.LexicalParent;
            }

            return ErrorHandlers.Error("Unbound variable: " + symbol);
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
        internal Obj Set(Obj var, Obj val)
        {
            if (!(var is string))
            {
                return ErrorHandlers.Error("Attempt to set a non-symbol: " + SchemeString.AsString(var));
            }

            string symbol = (string)var;
            if (this.symbolTable.Update(symbol, val))
            {
                return val;
            }

            if (this.LexicalParent != Empty)
            {
                return this.LexicalParent.Set(symbol, val);
            }

            return ErrorHandlers.Error("Unbound variable in set!: " + symbol);
        }

        /// <summary>
        /// Define a primitive, taking a variable number of arguments.
        /// Creates a Primitive and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        internal Environment DefinePrimitive(string name, Op operation, int minArgs, int maxArgs)
        {
            this.Define(name, new Primitive(operation, minArgs, maxArgs));
            return this;
        }

        /// <summary>
        /// Define a primitive, taking a fixed number of arguments.
        /// Creates a Primitive and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="numberOfArgs">The number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        internal Environment DefinePrimitive(string name, Op operation, int numberOfArgs)
        {
            return this.DefinePrimitive(name, operation, numberOfArgs, numberOfArgs);
        }

        /// <summary>
        /// Dump the stack of environments.
        /// At each level, sow the symbol table.
        /// </summary>
        /// <param name="levels">The number of levels to show.</param>
        /// <param name="indent">The number of characters to indent.</param>
        /// <returns>The environment stack, as a string.</returns>
        internal string Dump(int levels, int indent)
        {
            StringBuilder sb = new StringBuilder();
            Environment env = this;
            while (env != Empty && levels > 0)
            {
                env.symbolTable.Dump(indent, sb);
                levels--;
                if (levels > 0)
                {
                    sb.Append("-----\n");
                }

                env = env.LexicalParent;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Dump the environment.
        /// </summary>
        internal void DumpEnv()
        {
            Console.Out.WriteLine(this.Dump(100, 0));
        }

        /// <summary>
        /// Dump the top level environment.
        /// </summary>
        /// <returns>The environment, as a string.</returns>
        internal string Dump()
        {
            return this.Dump(1, 0);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Check that the variable and value lists have the same length.
        /// Iterate down the lists together.
        /// If the formals list is not a proper list, but ends in a dotted pair whose 
        ///   tail is a string, then the variable matches the rest of the args.
        /// </summary>
        /// <param name="vars">The variable list</param>
        /// <param name="vals">The value list</param>
        /// <param name="count">The number of variables in the list</param>
        /// <returns>True if the lists are both empty lists, if the variable list is just a string, 
        /// or if they are both lists of the same length.</returns>
        private static bool CheckArgCount(Obj vars, Obj vals, out int count)
        {
            count = 0;
            while (true)
            {
                if ((vars == List.Empty && vals == List.Empty) || (vars is string))
                {
                    return true;
                }

                if (!(vars is Pair && vals is Pair))
                {
                    return false;
                }

                vars = Rest(vars);
                vals = Rest(vals);
                count++;
            }
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
            private readonly Dictionary<string, Obj> symbolTable;

            /// <summary>
            /// Initializes a new instance of the Environment.SymbolTable class.
            /// </summary>
            /// <param name="count">The number of symbol table slots to pre-allocate.</param>
            internal SymbolTable(int count)
            {
                this.symbolTable = new Dictionary<string, Obj>(count);
            }

            /// <summary>
            /// Look up a symbol given its name.
            /// </summary>
            /// <param name="symbol">The symbol to look up.</param>
            /// <param name="val">Its returned value.</param>
            /// <returns>True if found in the symbol table, false if not found.</returns>
            internal bool Lookup(string symbol, out Obj val)
            {
                if (this.symbolTable.TryGetValue(symbol, out val))
                {
                    return true;
                }

                val = null;  // not used
                return false;
            }

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            internal void Add(string symbol, Obj val)
            {
                this.symbolTable[symbol] = val;
            }

            /// <summary>
            /// Add a list of symbols and values.
            /// The list of symbols and their values must be the same length.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            internal void AddList(Obj symbols, Obj vals)
            {
                while (symbols != List.Empty)
                {
                    if (symbols is string)
                    {
                        this.Add((string)symbols, vals);
                    }
                    else
                    {
                        Obj symbol = First(symbols);
                        if (!(symbol is string))
                        {
                            ErrorHandlers.Error("Bad formal parameter: " + symbol);
                        }

                        this.Add((string)symbol, First(vals));
                    }

                    symbols = Rest(symbols);
                    vals = Rest(vals);
                }
            }

            /// <summary>
            /// Update a value in the symbol table.
            /// </summary>
            /// <param name="symbol">The symbol to update.</param>
            /// <param name="val">The new value.</param>
            /// <returns>True if the value was found and stored, false if not found.</returns>
            internal bool Update(string symbol, Obj val)
            {
                if (this.symbolTable.ContainsKey(symbol))
                {
                    this.symbolTable[symbol] = val;
                    return true;
                }

                return false;
            }

            /// <summary>
            /// Dump the symbol table to a string for printing.
            /// </summary>
            /// <param name="indent">The number of characters to indent.</param>
            /// <param name="sb">A string builder to write the dump into.</param>
            internal void Dump(int indent, StringBuilder sb)
            {
                string initial = new string(' ', indent);
                foreach (var key in this.symbolTable.Keys)
                {
                    sb.AppendFormat("{0}{1}: {2}\n", initial, key, this.symbolTable[key]);
                }
            }
        }
        #endregion
    }
}