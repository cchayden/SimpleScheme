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
        /// Apply the primitive to the arguments, giving a result.
        /// As a convenience for primitives, they are allowed to return either
        ///   a result or a Stepper.  If they return a stepper, it means the result
        ///   is not yet ready, and that a new stepper was created and returned.  When
        ///   that stepper has a result, it will put in into ReturnedResult and return to the
        ///   caller stepper provided to it.
        /// If there is a result available immediately, this returns it by storing it
        ///   in ReturnedResult and returning to the caller.
        /// </summary>
        /// <param name="caller">The calling Stepper.</param>
        /// <param name="args">The arguments to the primitive.</param>
        /// <returns>The next step to execute.</returns>
        public override Stepper Apply(Stepper caller, object args)
        {
            // First check the number of arguments
            int numArgs = List.Length(args);
            if (numArgs < this.minArgs)
            {
                return ErrorHandlers.EvalError("Primitive: too few args, " + numArgs + ", for " +
                             this.Name + ": " + args);
            }

            if (numArgs > this.maxArgs)
            {
                return ErrorHandlers.EvalError("Primitive: too many args, " + numArgs + ", for " +
                             this.Name + ": " + args);
            }

            // Execute the operation
            object res = this.operation(caller, args);

            // See if the operation returns a result or another step
            if (res is Stepper)
            {
                return (Stepper)res;
            }

            // Operation returned a result -- just return this
            //  to the caller.
            return caller.ContinueStep(res);
        }
    }
}