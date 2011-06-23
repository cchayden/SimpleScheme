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
        }

        /// <summary>
        /// Call a continuation evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The continuation evaluator.</returns>
        public static EvaluateContinuation Call(Stepper caller, object expr)
        {
            return new EvaluateContinuation(caller, expr, caller.Env);            
        }

        /// <summary>
        /// Evaluate acontinuation
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            return ReturnFromStep(this.Expr);
        }
    }
}