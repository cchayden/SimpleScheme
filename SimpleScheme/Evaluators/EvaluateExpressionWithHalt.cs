// <copyright file="EvaluateExpressionWithHalt.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate an expression with the ability to halt.
    /// Normally the evaluator returns to the caller.
    /// But it also supports the option to halt rather than return.
    /// This option is selected at any time after evaluation has started.
    /// </summary>
    public sealed class EvaluateExpressionWithHalt : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-expression-with-halt";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateExpressionWithHalt class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateExpressionWithHalt(Obj expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates an expression evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The expression evaluator.</returns>
        public static EvaluateExpressionWithHalt Call(Obj expr, Environment env, Evaluator caller)
        {
            return new EvaluateExpressionWithHalt(expr, env, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Instead of returning to the caller, halt the computation after it completes.
        /// </summary>
        public void HaltAfterCompletion()
        {
            this.UpdateCaller(Interp.Halted);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating the expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Steps to evaluate the expression.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            return EvaluateExpression.Call(s.Expr, s.Env, s.ContinueHere(DoneStep));
        }

        /// <summary>
        /// Back here after the expression has been evaluated.
        /// Return to caller, or the halted expression that has overridden it.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with the return.</returns>
        private static Evaluator DoneStep(Evaluator s)
        {
            return s.ReturnFromStep(s.ReturnedExpr);
        }
        #endregion
    }
}