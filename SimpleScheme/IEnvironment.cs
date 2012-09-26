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
        /// Look up a variable in the environment.
        /// </summary>
        /// <param name="var">This must be a symbol.</param>
        /// <returns>The value of the symbol</returns>
        SchemeObject Lookup(string var);
    }
}
