﻿// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Primitive procedures.
    /// This contains implementations for all primitive procedures.
    /// Each primitive knows its operation, a reference to the code to execute to carry out
    ///   the primitive.  It also knows the min and max number of arguments it expects.
    /// Each instance of Primitive is immutable.
    /// </summary>
    public sealed class Primitive : Procedure
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "primitive";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The code to perform the operation.
        /// The stepper function is executed to perform the primitive operation.
        /// It takes two arguments, a caller and args.
        /// The caller is the step to return to when the operation is done.
        /// The args is the operand.
        /// The return value is either
        /// (1) a value, the operation result, or
        /// (2) a Stepper, the next step to execute. 
        /// </summary>
        private readonly Op operation;

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
        public Primitive(Op operation, int minArgs, int maxArgs)
        {
            this.operation = operation;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }

        /// <summary>
        /// The signature for primitives.
        /// </summary>
        /// <param name="caller">The calling stepper.</param>
        /// <param name="args">The primitive's arguments</param>
        /// <returns>The primitive's result.</returns>
        public delegate Obj Op(Stepper caller, Obj args);

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
        /// <param name="args">The arguments to the primitive.</param>
        /// <param name="caller">The calling Stepper.</param>
        /// <returns>The next step to execute.</returns>
        public override Stepper Apply(object args, Stepper caller)
        {
            // First check the number of arguments
            int numArgs = Length(args);
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

            caller.Env.Interp.IncrementCounter(counter);

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

        /// <summary>
        /// The string form of a proc is its name in curly brackets.
        /// </summary>
        /// <returns>The name of the proc.</returns>
        public override string ToString()
        {
            return "{" + this.Name + "}";
        }
    }
}