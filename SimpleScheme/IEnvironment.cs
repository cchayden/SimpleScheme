// <copyright file="IEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The interface for the SimpleScheme environment.
    /// </summary>
    public interface IEnvironment
    {
        /// <summary>
        /// Install primitives into the environment.
        /// This is for setting up the primitive environment.
        /// </summary>
        void InstallPrimitives();

        /// <summary>
        /// Define a variable in the environment.
        /// </summary>
        /// <param name="var">This must be a symbol.</param>
        /// <param name="val">The value of the variable.</param>
        void Define(object var, object val);

        /// <summary>
        /// Define a primitive in the environment.
        /// </summary>
        /// <param name="name">The primitive name.  Must be a symbol.</param>
        /// <param name="operation">A function that performs the primitive operation.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <returns>The environment.</returns>
        IEnvironment DefinePrim(object name, Primitive.Op operation, int minArgs, int maxArgs);

        /// <summary>
        /// Define a primitive in the environment.
        /// </summary>
        /// <param name="name">The primitive name.  Must be a symbol.</param>
        /// <param name="operation">A function that performs the primitive operation.</param>
        /// <param name="numberOfArgs">The number of arguments.</param>
        /// <returns>The environment.</returns>
        IEnvironment DefinePrim(object name, Primitive.Op operation, int numberOfArgs);
    }
}
