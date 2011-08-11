// <copyright file="EvaluateParallel.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    using Obj = System.Object;

    /// <summary>
    /// Evaluate a sequence of exprs in parallel by evaluating each member.
    /// </summary>
    public sealed class EvaluateParallel : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-parallel";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The list of expressions.
        /// </summary>
        private Obj expressions;

        /// <summary>
        /// The step we call to evaluate an expression in parallel.
        /// </summary>
        private EvaluateIdentity pendingEvaluation;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateParallel class.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateParallel(Obj expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            this.expressions = expr;
            ContinueHere(EvalExprStep);
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
        /// Call the parallel evaluator.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The parallel evaluator.</returns>
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller)
        {
            return new EvaluateParallel(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Initial step: see if we are done.
        /// If not, evaluate the next expression.
        /// If we are, evaluate and return.
        /// If the expression suspended, then go on anyway with the next expr.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator.</returns>
        private static Evaluator EvalExprStep(Evaluator s)
        {
            EvaluateParallel step = (EvaluateParallel)s;
            if (EmptyList.Is(step.expressions))
            {
                return s.ReturnFromStep(Undefined.Instance);
            }

            // We really want to de EvaluateExpression, but need the ability to cancel it.
            // Use the IdentityEvaluator, which keeps its caller reference to itself, so allows
            //  cancellation.
            step.pendingEvaluation = EvaluateIdentity.Call(List.First(step.expressions), s.Env, s.ContinueHere(LoopStep));
            return step.pendingEvaluation;
        }

        /// <summary>
        /// Comes back here after suspension.
        /// This happens because CatchSuspended is true and something in the EvaluateExpression suspended.
        /// Loop back and evaluate another expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Immediately steps back.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            EvaluateParallel step = (EvaluateParallel)s;

            // If we came back normally, then this has not effect.
            // If we came back by catching suspended, this makes sure that after resumption,
            //    the evaluation stops after completing the expression.
            step.pendingEvaluation.HaltAfterCompletion();
            step.expressions = List.Rest(step.expressions);
            return s.ContinueHere(EvalExprStep);
        }
        #endregion
    }
}