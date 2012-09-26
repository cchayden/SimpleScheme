// <copyright file="EvaluateTime.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate an expression while timing it.
    /// This may evaluate the expression multiple times.
    /// </summary>
    internal sealed class EvaluateTime : EvaluateTimeBase
    {
        #region Fields
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper doneStep = GetStepper("DoneStep");

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
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
        }
        #endregion

        #region Call
        /// <summary>
        /// Call a timed evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The timed evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return new EvaluateTime(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the given expression.  
        /// This evaluates the expression that is being timed.
        /// Test to see if we are done.
        /// </summary>
        /// <returns>If done, the result.  Otherwise, continue to next step.</returns>
        protected override Evaluator EvaluateStep()
        {
            this.Pc = doneStep;
            return EvaluateExpression.Call(First(this.Expr), this.Env, this);
        }
        #endregion
    }
}
