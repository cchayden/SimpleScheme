// <copyright file="EvaluatorContinuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate a continuation
        /// Capture the value to return and supply a step to resume.
        /// </summary>
        private class EvaluatorContinuation : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorContinuation class.
            /// </summary>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorContinuation(Stepper parent, object expr, Environment env)
                : base(parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate acontinuation
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper RunStep()
            {
                return SubReturn(this.Expr);
            }
        }
    }
}