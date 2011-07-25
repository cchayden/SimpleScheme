// <copyright file="EvaluateContinuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a continuation
    /// Capture the value to return and supply a step to resume.
    /// </summary>
    internal sealed class EvaluateContinuation : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        internal const string StepperName = "evaluate-continuation";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateContinuation class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateContinuation(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Call a continuation evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The continuation evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateContinuation(expr, env, caller);            
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// The only step, a continuation returns the saved expression.
        /// The caller that it returns to is really the step to continue from, not the current caller.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The expression.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            return s.ReturnFromStep(s.Expr);
        }
        #endregion
    }
}