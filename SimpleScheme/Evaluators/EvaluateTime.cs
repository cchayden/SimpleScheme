// <copyright file="EvaluateTime.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate an expression while timing it.
    /// This may evaluate the expression multiple times.
    /// </summary>
    public sealed class EvaluateTime : EvaluateTimeBase
    {
        #region Fields

        /// <summary>
        /// The symbol "time"
        /// </summary>
        public static readonly Symbol TimeSym = "time";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-time");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateTime class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateTime(SchemeObject expr, Environment env, Evaluator caller)
            : base(expr, 1, env, caller, counter)
        {
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
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            return new EvaluateTime(expr, env, caller);
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Evaluate the given expression.  
        /// This evaluates the expression that is being timed.
        /// Test to see if we are done.
        /// </summary>
        /// <returns>If done, the result.  Otherwise, continue to next step.</returns>
        protected override Evaluator Step1()
        {
            return EvaluateExpression.Call(First(this.Expr), this.Env, this.ContinueAt(Step2));
        }
        #endregion
    }
}
