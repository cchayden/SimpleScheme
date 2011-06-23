// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.CodeAnalysis;
    using System.IO;
    using System.Reflection;

    /// <summary>
    /// Primitive procedures.
    /// This contains implementations for all primitive procedures.
    /// Each primitive knows its operCode, and the min and max number of arguments it expects.
    /// Each instance of Primitive is immutable.  In practice, we have one instance for each operCode.
    /// </summary>
    public sealed class Primitive : Procedure
    {
        /// <summary>
        /// The code to perform the operation.
        /// </summary>
        private readonly Func<Stepper, object, object> operation;

        /// <summary>
        /// The minimum number of arguments expected.
        /// </summary>
        private readonly int minArgs;

        /// <summary>
        /// The maximum number of args expected.
        /// </summary>
        private readonly int maxArgs;

        /// <summary>
        /// Initializes a new instance of the Primitive class.
        /// </summary>
        /// <param name="operation">The code to carry out the operation.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        public Primitive(Func<Stepper, object, object> operation, int minArgs, int maxArgs)
        {
            this.operation = operation;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }

        /// <summary>
        /// Do all the combination car-cdr functions.
        /// </summary>
        /// <param name="name">The function name.</param>
        /// <param name="args">The expression to operate on.</param>
        /// <returns>The result of the operation.</returns>
        public static object Cxr(string name, object args)
        {
            object first = List.First(args);
            for (int i = name.Length - 2; i >= 1; i--)
            {
                first = name[i] == 'a' ? List.First(first) : List.Rest(first);
            }

            return first;
        }

        /// <summary>
        /// Apply the primitive to the arguments, giving a result.
        /// This may return a result or a Stepper, which can be used to get a result.
        /// If parent is null, then it will not return a Stepper, so when that is not
        ///   acceptable, pass null for parent.
        /// </summary>
        /// <param name="parent">The calling Stepper.</param>
        /// <param name="args">The arguments to the primitive.</param>
        /// <returns>The result of the application.</returns>
        public override object Apply(Stepper parent, object args)
        {
            // First check the number of arguments
            int numArgs = List.Length(args);
            if (numArgs < this.minArgs)
            {
                return ErrorHandlers.Error("Primitive: too few args, " + numArgs + ", for " +
                             this.Name + ": " + args);
            }

            if (numArgs > this.maxArgs)
            {
                return ErrorHandlers.Error("Primitive: too many args, " + numArgs + ", for " +
                             this.Name + ": " + args);
            }

            // Execute the operation
            return this.operation(parent, args);
        }
    }
}