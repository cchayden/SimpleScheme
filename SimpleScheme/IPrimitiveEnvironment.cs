// <copyright file="IPrimitiveEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The interface for the SimpleScheme primitive environment.
    /// The primitive environment holds the primitive definitions, and is the
    ///   parent of the global environment.  This allows it to be shared between
    ///   different interpreter instances.
    /// </summary>
    public interface IPrimitiveEnvironment : IEnvironment
    {
        /// <summary>
        /// Define a primitive in the environment.
        /// </summary>
        /// <param name="name">The primitive name.  Must be a symbol.</param>
        /// <param name="operation">A function that performs the primitive operation.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>The environment.</returns>
        IPrimitiveEnvironment DefinePrim(object name, Primitive.Op operation, int minArgs, int maxArgs);

        /// <summary>
        /// Define a primitive in the environment.
        /// </summary>
        /// <param name="name">The primitive name.  Must be a symbol.</param>
        /// <param name="operation">A function that performs the primitive operation.</param>
        /// <param name="numberOfArgs">The number of arguments.</param>
        /// <returns>The environment.</returns>
        IPrimitiveEnvironment DefinePrim(object name, Primitive.Op operation, int numberOfArgs);
    }
}