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
            ContinueHere(EvalExprStep);
            IncrementCounter(counter);
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
        /// Evaluate expression step: see if we are done.
        /// If we are, return undefined.
        /// If we are, evaluate the next expression.
        /// Instead of calling normal EvaluateExpression, call a variant that catches suspended
        ///   execution and halts the evaluation.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator.</returns>
        private static Evaluator EvalExprStep(Evaluator s)
        {
            if (EmptyList.Is(s.Expr))
            {
                return s.ReturnFromStep(Undefined.Instance);
            }

            return EvaluateExpressionWithCatch.Call(List.First(s.Expr), s.Env, s.ContinueHere(LoopStep));
        }

        /// <summary>
        /// Comes back here after evaluation completes synchronously or is suspended.
        /// In either case, returned value is discarded.
        /// If evaluation is suspended, then EvaluateExpressionWithCatch will catch and return undefined.
        /// Loop back and evaluate another expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Immediately steps back.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            s.UpdateExpr(List.Rest(s.Expr));
            return s.ContinueHere(EvalExprStep);
        }
        #endregion
    }
}