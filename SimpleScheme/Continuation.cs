#define OLD
// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Represents a continuation.
    /// This only works in a restricted sense, since it is implemented by throwing and 
    ///    catching an exception.
    /// </summary>
    public class Continuation : Procedure
    {
        /// <summary>
        /// The exception to throw.
        /// </summary>
        private readonly Stepper step;

        /// <summary>
        /// Initializes a new instance of the Continuation class.
        /// </summary>
        /// <param name="cc">The exception to throw.</param>
        /// <param name="step">The continuation to return to when applied.</param>
        public Continuation(Stepper step)
        {
            this.step = step;
        }

        /// <summary>
        /// Gets the value to return as the result of executing the continuation.
        /// </summary>
        public object Value { get; private set; }

        /// <summary>
        /// Execute the continuation.
        /// </summary>
        /// <param name="interpreter">The interpreter to execute in.</param>
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">The value to return.</param>
        /// <returns>The result of applying the continuation.</returns>
        public override object Apply(Scheme interpreter, Stepper parent, object args)
        {
            this.Value = First(args);
            return Stepper.SubTransfer(First(args), this.step.Env, this.step.Parent.Parent);
        }
    }
}
