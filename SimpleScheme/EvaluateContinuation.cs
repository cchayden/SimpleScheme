// <copyright file="EvaluateContinuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a continuation
    /// Capture the value to return and supply a step to resume.
    /// </summary>
    public sealed class EvaluateContinuation : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateContinuation class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateContinuation(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.InitialStep;
            IncrementCounter("continuation");
        }

        /// <summary>
        /// Call a continuation evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The continuation evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateContinuation(caller, expr, caller.Env);            
        }

        /// <summary>
        /// The only step, a continuation returns the saved expression.
        /// The parent that it returns to is really the step to continue from, not the current caller.
        /// </summary>
        /// <returns>The expression.</returns>
        private Stepper InitialStep()
        {
            return ReturnFromStep(this.Expr);            
        }
    }
}