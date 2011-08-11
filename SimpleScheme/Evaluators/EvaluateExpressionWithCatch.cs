// <copyright file="EvaluateExpressionWithCatch.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate an expression with the ability to catch suspension.
    /// Anything that this calls that returns a suspended evaluator will be "caught"
    ///   by this evaluator.
    /// </summary>
    public sealed class EvaluateExpressionWithCatch : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-expression-with-catch";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The step we call to evaluate an expression in parallel.
        /// </summary>
        private EvaluateExpressionWithHalt pendingEvaluation;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateExpressionWithCatch class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateExpressionWithCatch(Obj expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Catch when a caller suspends
        /// </summary>
        /// <returns>We want to catch suspensions.</returns>
        public override bool CatchSuspended
        {
            get { return true; }
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
        public static EvaluateExpressionWithCatch Call(Obj expr, Environment env, Evaluator caller)
        {
            return new EvaluateExpressionWithCatch(expr, env, caller);
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
            EvaluateExpressionWithCatch step = (EvaluateExpressionWithCatch)s;
            step.pendingEvaluation = EvaluateExpressionWithHalt.Call(s.Expr, s.Env, s.ContinueHere(DoneStep));
            return step.pendingEvaluation;
        }

        /// <summary>
        /// Back here after the expression has been evaluated.
        /// Return to caller, or the halted expression that has overridden it.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with the return.</returns>
        private static Evaluator DoneStep(Evaluator s)
        {
            EvaluateExpressionWithCatch step = (EvaluateExpressionWithCatch)s;

            // If we came back normally, then this has no effect.
            // If we came back by catching suspended, this makes sure that after resumption,
            //    the evaluation stops after completing the expression.
            step.pendingEvaluation.HaltAfterCompletion();
            return s.ReturnFromStep(s.ReturnedExpr);
        }
        #endregion
    }
}