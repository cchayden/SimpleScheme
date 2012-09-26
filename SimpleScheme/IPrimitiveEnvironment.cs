// <copyright file="IPrimitiveEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The function that executes a primitive.
    /// </summary>
    /// <param name="args">The operation arguments.</param>
    /// <param name="env">The operation environment.</param>
    /// <param name="caller">The calling evaluator.</param>
    /// <returns>Returns a SchemeObject (the primitive result) or an Evaluator, which
    /// can be called to get the result.</returns>
    public delegate EvaluatorOrObject Operation(SchemeObject args, Environment env, Evaluator caller);

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
        /// <param name="description">A description of the primitive.</param>
        /// <param name="operation">A function that performs the primitive operation.</param>
        /// <param name="argsInfo">Information about primitive args.</param>
        /// <returns>The environment.</returns>
        IPrimitiveEnvironment DefinePrimitive(Symbol name, string[] description, Operation operation, ArgsInfo argsInfo);
    }

    /// <summary>
    /// Encapsulates information about the arguments passed to a procedure.
    /// Includes max and min argument count, whether the arguments should be evaluated, and the
    ///   required argument types.
    /// </summary>
    public struct ArgsInfo
    {
        /// <summary>
        /// The minimum number of arguments permitted.
        /// </summary>
        private readonly int minArgs;

        /// <summary>
        /// The maximum number of arguments permitted.
        /// </summary>
        private readonly int maxArgs;

        /// <summary>
        /// True if the arguments should be unevaluated.
        /// </summary>
        private readonly bool unevaluated;

        /// <summary>
        /// The argument types.
        /// If there are fewer of these than actual arguments, the last one is repeated.
        /// </summary>
        private readonly ArgType[] argTypes;

        /// <summary>
        /// Initializes a new instance of the ArgsInfo struct.
        /// </summary>
        /// <param name="numArgs">The number of arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        public ArgsInfo(int numArgs, params ArgType[] argTypes) : 
            this(numArgs, numArgs, false, argTypes)
        {
        }

        /// <summary>
        /// Initializes a new instance of the ArgsInfo struct.
        /// </summary>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        public ArgsInfo(int minArgs, int maxArgs, params ArgType[] argTypes) : 
            this(minArgs, maxArgs, false, argTypes)
        {
        }

        /// <summary>
        /// Initializes a new instance of the ArgsInfo struct.
        /// </summary>
        /// <param name="numArgs">The number of arguments.</param>
        /// <param name="unevaluated">Flag indicating whether to evaluate arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        public ArgsInfo(int numArgs, bool unevaluated, params ArgType[] argTypes) : 
            this(numArgs, numArgs, unevaluated, argTypes)
        {
        }

        /// <summary>
        /// Initializes a new instance of the ArgsInfo struct.
        /// </summary>
        /// <param name="unevaluated">Flag indicating whether to evaluate arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        public ArgsInfo(bool unevaluated, params ArgType[] argTypes) : 
            this(0, int.MaxValue, unevaluated, argTypes)
        {
        }

        /// <summary>
        /// Initializes a new instance of the ArgsInfo struct.
        /// </summary>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        /// <param name="unevaluated">Flag indicating whether to evaluate arguments.</param>
        /// <param name="argTypes">The argument types.</param>
        public ArgsInfo(int minArgs, int maxArgs, bool unevaluated, params ArgType[] argTypes) : 
            this()
        {
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
            this.unevaluated = unevaluated;
            this.argTypes = argTypes;
        }

        /// <summary>
        /// Initializes a new instance of the ArgsInfo struct.
        /// </summary>
        /// <param name="args">The minimum and maximum number of arguments.</param>
        public ArgsInfo(int[] args) : 
            this(args[0], args[1], false)
        {
        }

        /// <summary>
        /// Gets the minimum number of arguments.
        /// </summary>
        internal int MinArgs
        {
            get { return this.minArgs; }
        }

        /// <summary>
        /// Gets the maximum number of arguments.
        /// </summary>
        internal int MaxArgs
        {
            get { return this.maxArgs; }
        }

        /// <summary>
        /// Gets a value indicating whether the arguments should be evaluated.
        /// </summary>
        internal bool Unevaluated
        {
            get { return this.unevaluated; }
        }

        /// <summary>
        /// Gets the argument types
        /// </summary>
        internal ArgType[] ArgTypes
        {
            get { return this.argTypes; }
        }
    }
}