// <copyright file="IEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The environment interface.
    /// The only operation available on the interface is to define an object in the environment.
    /// </summary>
    public interface IEnvironment
    {
        /// <summary>
        /// Define a variable in the environment.
        /// </summary>
        /// <param name="var">This must be a symbol.</param>
        /// <param name="val">The value of the variable.</param>
        void Define(string var, SchemeObject val);

        /// <summary>
        /// Set the value of a variable in the environment to a new value.
        /// Var is the variable name.
        /// Searches the chain of environments looking for a binding.
        /// </summary>
        /// <param name="var">The variable name.</param>
        /// <param name="val">The new value for the variable.</param>
        void Set(string var, SchemeObject val);

            /// <summary>
        /// Look up a variable in the environment.
        /// </summary>
        /// <param name="var">This must be a symbol.</param>
        /// <returns>The value of the symbol</returns>
        SchemeObject Lookup(string var);
    }
}
