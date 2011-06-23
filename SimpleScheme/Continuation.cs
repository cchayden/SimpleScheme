// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Represents a continuation.
    /// </summary>
    public class Continuation : Procedure
    {
        /// <summary>
        /// The step to execute when the continuation is applied.
        /// </summary>
        private readonly Stepper step;

        /// <summary>
        /// Initializes a new instance of the Continuation class.
        /// </summary>
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
        /// <param name="parent">The calling evaluator.</param>
        /// <param name="args">The value to return.</param>
        /// <returns>The result of applying the continuation.</returns>
        public override object Apply(Stepper parent, object args)
        {
            this.Value = First(args);
            return Stepper.TransferToStep(First(args), this.step.Env, this.step.Parent.Parent);
        }

        /// <summary>
        /// Display the continuation as a string.  
        /// Displays the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the continuation.</returns>
        public override string ToString()
        {
            return string.Format("call-with-current-continuation {0}", this.step.Expr);
        }
    }
}
