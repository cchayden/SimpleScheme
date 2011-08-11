// <copyright file="EvaluateIdentity.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Identity Evaluator
    /// This exists so it can be canceled.
    /// </summary>
    public sealed class EvaluateIdentity : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-identity";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateIdentity class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateIdentity(Obj expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates an identity evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The if evaluator.</returns>
        public static EvaluateIdentity Call(Obj expr, Environment env, Evaluator caller)
        {
            return new EvaluateIdentity(expr, env, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Instead of returning to the caller, halt the computation after this completes.
        /// </summary>
        public void HaltAfterCompletion()
        {
            this.UpdateCaller(Interp.Halted);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Steps to evaluate the test.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            return EvaluateExpression.Call(s.Expr, s.Env, s.ContinueHere(DoneStep));
        }

        /// <summary>
        /// Back here after the test has been evaluated.
        /// Evaluate and return either the second or third expression.
        /// If there is no thid, the empty list will be evaluated, which is OK.
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