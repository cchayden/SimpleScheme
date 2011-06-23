// <copyright file="Environment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Text;

    /// <summary>
    /// Represents the interpreter environment.
    /// The environment is part of a chain, made up of the Parent links, that represents the dynamic context.
    /// At the bottom of the chain is the global environment, created by the single-arg constructor.
    /// Each link in the chain contains a symbol table of the bindings at that level.
    /// Each lookup searches down the symbol tables in the chain, from the top to the bottom.
    /// </summary>
    public sealed class Environment
    {
        /// <summary>
        /// The symbol table for this enviornment.
        /// </summary>
        private readonly SymbolTable symbolTable = new SymbolTable();

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// This is used to create the primitive environment.
        /// </summary>
        public Environment()
        {
        }
        
        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// This is used to create the global environment.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent environment.</param>
        public Environment(Interpreter interp, Environment parent)
        {
            this.Interp = interp;
            this.Parent = parent;
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// Start out with a set of variable bindings and a parent environment.
        /// The initial variable bindings are the formal parameters and the corresponding argument values.
        /// </summary>
        /// <param name="vars">A list of variable names.</param>
        /// <param name="vals">The values for these variables.</param>
        /// <param name="parent">The parent environment.</param>
        public Environment(object vars, object vals, Environment parent)
        {
            this.Parent = parent;
            this.Interp = parent.Interp;
            if (!NumberArgsOk(vars, vals))
            {
                ErrorHandlers.Warn("Wrong number of arguments: expected " + vars + " got " + vals);
            }

            this.symbolTable.AddList(vars, vals);
        }

        /// <summary>
        /// Gets the interpreter used to hold the global context.
        /// The bottom environment holds the interpreter, but each one copies from its
        ///   parent, so that it can be accessed directly.
        /// This field is written only in the constructor -- it is never modified.
        /// </summary>
        public Interpreter Interp { get; private set; }

        /// <summary>
        /// Gets the parent environment.
        /// </summary>
        public Environment Parent { get; private set; }

        /// <summary>
        /// Install primitives into the environment.
        /// </summary>
        /// <returns>The environment.</returns>
        public Environment InstallPrimitives()
        {
            Number.DefinePrimitives(this);
            Procedure.DefinePrimitives(this);
            List.DefinePrimitives(this);
            InputPort.DefinePrimitives(this);
            OutputPort.DefinePrimitives(this);
            Vector.DefinePrimitives(this);
            SchemeBoolean.DefinePrimitives(this);
            SchemeString.DefinePrimitives(this);
            ClrProcedure.DefinePrimitives(this);
            SynchronousClrProcedure.DefinePrimitives(this);
            AsynchronousClrProcedure.DefinePrimitives(this);
            ErrorHandlers.DefinePrimitives(this);

            this
                .DefinePrimitive(
                   "exit", 
                   (parent, args) =>
                        {
                            System.Environment.Exit(List.First(args) == null ? 0 : (int)Number.Num(List.First(args)));
                            return null;
                        },
                   0, 
                   1)
                .DefinePrimitive("time-call", (parent, args) => EvaluateTimeCall.Call(parent, args), 1, 2);

            return this;
        }

        /// <summary>
        /// Add a new definition into the environment.
        /// If a procedure is being added, set its name.
        /// </summary>
        /// <param name="var">This is a variable to add to the environment.</param>
        /// <param name="val">This is the value of that variable.</param>
        /// <returns>The variable added to the environment.</returns>
        public object Define(object var, object val)
        {
            this.symbolTable.Add(var, val);

            if (val is Procedure)
            {
                ((Procedure)val).SetName(var.ToString());
            }
            
            return var;
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// </summary>
        /// <param name="symbol">The name of the variable to look up.</param>
        /// <returns>The value bound to the variable.</returns>
        public object Lookup(string symbol)
        {
            object val;
            if (this.symbolTable.Lookup(symbol, out val))
            {
                return val;
            }

            // if we get here, we have not found anything, so look in the parent
            if (this.Parent != null)
            {
                return this.Parent.Lookup(symbol);
            }

            return ErrorHandlers.Error("Lookup: unbound variable: " + symbol);
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
        public object Set(object var, object val)
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

            if (this.Parent != null)
            {
                return this.Parent.Set(symbol, val);
            }

            return ErrorHandlers.Error("Unbound variable: " + symbol);
        }

        /// <summary>
        /// Define a primitive, taking a variable number of arguments.
        /// Creates a Primitive object and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public Environment DefinePrimitive(string name, Func<Stepper, object, object> operation, int minArgs, int maxArgs)
        {
            this.Define(name, new Primitive(operation, minArgs, maxArgs));
            return this;
        }

        /// <summary>
        /// Define a primitive, taking a fixed number of arguments.
        /// Creates a Primitive object and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="operation">The operation to perform.</param>
        /// <param name="numberOfArgs">The number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public Environment DefinePrimitive(string name, Func<Stepper, object, object> operation, int numberOfArgs)
        {
            return this.DefinePrimitive(name, operation, numberOfArgs, numberOfArgs);
        }

        /// <summary>
        /// Dump the stack of environments.
        /// At each level, sow the symbol table.
        /// </summary>
        /// <param name="nLevels">The number of levels to show.</param>
        /// <returns>The environment stack, as a string.</returns>
        public string Dump(int nLevels)
        {
            StringBuilder sb = new StringBuilder();
            Environment env = this;
            while (env != null && nLevels > 0)
            {
                env.symbolTable.Dump(sb);
                sb.Append("-----\n");
                env = env.Parent;
                nLevels--;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Dump the top level environment.
        /// </summary>
        /// <returns>The environment, as a string.</returns>
        public string Dump()
        {
            return this.Dump(1);
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
        /// Check that the variable and value lists have the same length.
        /// Iterate down the lists together.
        /// If the vars list is not a proper list, but ends in a dotted pair whose 
        ///   tail is a string, then the variable matches the rest of the args.
        /// </summary>
        /// <param name="vars">The variable list</param>
        /// <param name="vals">The value list</param>
        /// <returns>True if the lists are both null, if the variable list is just a string, 
        /// or if they are both lists of the same length.</returns>
        private static bool NumberArgsOk(object vars, object vals)
        {
            while (true)
            {
                if ((vars == null && vals == null) || (vars is string))
                {
                    return true;
                }

                if (!(vars is Pair && vals is Pair))
                {
                    return false;
                }

                vars = List.Rest(vars);
                vals = List.Rest(vals);
            }
        }

        /// <summary>
        /// The symbol table for the environment.
        /// </summary>
        private class SymbolTable
        {
            /// <summary>
            /// Stores symbols and their values.
            /// </summary>
            private readonly Dictionary<object, object> symbolTable = new Dictionary<object, object>();

            /// <summary>
            /// Look up a symbol given its name.
            /// </summary>
            /// <param name="symbol">The symbol to look up.</param>
            /// <param name="val">Its returned value.</param>
            /// <returns>True if found in the symbol table, false if not found.</returns>
            public bool Lookup(object symbol, out object val)
            {
                if (this.symbolTable.ContainsKey(symbol))
                {
                    val = this.symbolTable[symbol];
                    return true;
                }

                val = null;
                return false;
            }

            /// <summary>
            /// Add a symbol and its value to the environment.
            /// </summary>
            /// <param name="symbol">The symbol name.</param>
            /// <param name="val">The value.</param>
            public void Add(object symbol, object val)
            {
                this.symbolTable[symbol] = val;
            }

            /// <summary>
            /// Add a list of symbols and values.
            /// The list of symbols and their values must be the same length.
            /// </summary>
            /// <param name="symbols">The list of symbols.</param>
            /// <param name="vals">The list of values.</param>
            public void AddList(object symbols, object vals)
            {
                while (symbols != null)
                {
                    if (symbols is string)
                    {
                        this.Add(symbols, vals);
                    }
                    else
                    {
                        this.Add(List.First(symbols), List.First(vals));
                    }

                    symbols = List.Rest(symbols);
                    vals = List.Rest(vals);
                }
            }

            /// <summary>
            /// Update a value in the symbol table.
            /// </summary>
            /// <param name="symbol">The symbol to update.</param>
            /// <param name="val">The new value.</param>
            /// <returns>True if the value was found and stored, false if not found.</returns>
            public bool Update(object symbol, object val)
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
            /// <param name="sb">A string builder to write the dump into.</param>
            public void Dump(StringBuilder sb)
            {
                foreach (var kvp in this.symbolTable)
                {
                    sb.AppendFormat("{0} {1}\n", kvp.Key, kvp.Value);
                }
            }
        }
    }
}