// <copyright file="Environment.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Represents the interpreter environment.
    /// </summary>
    public sealed class Environment : SchemeUtils
    {
        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// This is used to create the global environment.
        /// </summary>
        public Environment()
        {
        }

        /// <summary>
        /// Initializes a new instance of the Environment class.
        /// Start out with a set of variable bindings and a parent environment.
        /// The environment is kept in two lists, meaning that they have to be searched linearly 
        ///    to find anything.
        /// </summary>
        /// <param name="vars">A list of variable names.</param>
        /// <param name="vals">The values for these variables.</param>
        /// <param name="parent">The parent environment.</param>
        public Environment(object vars, object vals, Environment parent)
        {
            this.Vars = vars;
            this.Vals = vals;
            this.Parent = parent;

            if (!NumberArgsOk(vars, vals))
            {
                Warn("wrong number of arguments: expected " + vars + " got " + vals);
            }
        }

        /// <summary>
        /// Gets a list of the variables in the environment.
        /// This list and the Vals list must have the same number of members.
        /// They match up, so a variable in one list corresponds to a value at the same place 
        ///    in the other.
        /// </summary>
        public object Vars { get; private set; }

        /// <summary>
        /// Gets a list of the variable values in the environment.
        /// </summary>
        public object Vals { get; private set; }

        /// <summary>
        /// Gets the parent environment.
        /// </summary>
        public Environment Parent { get; private set; }

        /// <summary>
        /// Add a new definition into the environment.
        /// If a procedure is being added, set its name.
        /// </summary>
        /// <param name="var">This is a variable to add to the environment.</param>
        /// <param name="val">This is the value of that variable.</param>
        /// <returns>The variable added to the environment.</returns>
        public object Define(object var, object val)
        {
            this.Vars = Cons(var, this.Vars);
            this.Vals = Cons(val, this.Vals);

            if (val is Procedure)
            {
                ((Procedure)val).SetName(var.ToString());
            }
            
            return var;
        }

        /// <summary>
        /// Define a primitive, taking a fixed number of arguments.
        /// Creates a Primitive object and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="opCode">The opCode.</param>
        /// <param name="nArgs">The number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public Environment DefPrim(string name, Primitive.OpCode opCode, int nArgs)
        {
            this.Define(name, new Primitive(opCode, nArgs, nArgs));
            return this;
        }

        /// <summary>
        /// Define a primitive, taking a variable number of arguments.
        /// Creates a Primitive object and puts it in the environment associated 
        ///    with the given name.
        /// </summary>
        /// <param name="name">The primitive name.</param>
        /// <param name="opCode">The opCode.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>A refernce to the environment.</returns>
        public Environment DefPrim(
            string name, 
            Primitive.OpCode opCode, 
            int minArgs, 
            int maxArgs)
        {
            this.Define(name, new Primitive(opCode, minArgs, maxArgs));
            return this;
        }

        /// <summary>
        /// Look up a symbol in the environment.
        /// </summary>
        /// <param name="symbol">The name of the variable to look up.</param>
        /// <returns>The value bound to the variable.</returns>
        public object Lookup(string symbol)
        {
            object varlist = this.Vars;
            object vallist = this.Vals;

            // iterate down through the lists
            while (varlist != null)
            {
                // if the variable name matches, return the corresponding value
                if (First(varlist) as string == symbol)
                {
                    return First(vallist);
                }

                // if the list is just a string and it matches, return the value
                if (varlist as string == symbol)
                {
                    return vallist;
                }

                // iterate on both lists
                varlist = Rest(varlist);
                vallist = Rest(vallist);
            }

            // if we get here, we have not found anything, so look in the parent
            if (this.Parent != null)
            {
                return this.Parent.Lookup(symbol);
            }

            return Error("Unbound variable: " + symbol);
        }

        /// <summary>
        /// Set the value of a variable in the environment to a new value.
        /// Var must be a string, the variable name.
        /// Search down the list, and into the parentif necessary, to find the variable.
        /// Then set it to the new value.
        /// </summary>
        /// <param name="var">The variable name.</param>
        /// <param name="val">The new value for the variable.</param>
        /// <returns>The value that the variable was set to.</returns>
        public object Set(object var, object val)
        {
            if (!(var is string))
            {
                return Error("Attempt to set a non-symbol: " + Stringify(var));
            }

            string symbol = (string)var;
            object varList = this.Vars;
            object valList = this.Vals;

            while (varList != null)
            {
                if (First(varList) as string == symbol)
                {
                    return SetFirst(valList, val);
                }

                if (Rest(varList) as string == symbol)
                {
                    return SetRest(valList, val);
                }

                varList = Rest(varList);
                valList = Rest(valList);
            }

            if (this.Parent != null)
            {
                return this.Parent.Set(symbol, val);
            }

            return Error("Unbound variable: " + symbol);
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

                vars = Rest(vars);
                vals = Rest(vals);
            }
        }
    }
}