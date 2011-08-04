// <copyright file="EvaluateTimeCall.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate an expression while timing it..
    /// This can evaluate the expression multiple times.
    /// </summary>
    public sealed class EvaluateTimeCall : EvaluateTimeBase
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-time-call";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateTimeCall class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateTimeCall(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call a timed evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The timed evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateTimeCall(expr, env, caller);
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Evaluate the given expression.  
        /// This evaluates the expression that is being timed.
        /// Test to see if we are done.
        /// </summary>
        /// <returns>If done, the result.  Otherwise, continue to next step.</returns>
        protected override Stepper Step1()
        {
            return Procedure.As(List.First(Expr)).Apply(EmptyList.Instance, ContinueHere(Step2));
        }
        #endregion
    }
}
