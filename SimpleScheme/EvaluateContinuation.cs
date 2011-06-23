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
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-continuation";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Initializes a new instance of the EvaluateContinuation class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateContinuation(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
        }

        /// <summary>
        /// Call a continuation evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The continuation evaluator.</returns>
        public static Stepper Call(object expr, Stepper caller)
        {
            return new EvaluateContinuation(expr, caller.Env, caller);            
        }

        /// <summary>
        /// The only step, a continuation returns the saved expression.
        /// The caller that it returns to is really the step to continue from, not the current caller.
        /// </summary>
        /// <returns>The expression.</returns>
        private Stepper InitialStep()
        {
            return ReturnFromStep(Expr);            
        }
    }
}