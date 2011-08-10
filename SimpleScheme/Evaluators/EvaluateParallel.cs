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

        #region Properties
        /// <summary>
        /// Catch when a caller suspends
        /// </summary>
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

            return EvaluateExpression.Call(List.First(step.expressions), s.Env, s.ContinueHere(LoopStep));
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
            step.expressions = List.Rest(step.expressions);
            return s.ContinueHere(EvalExprStep);
        }
        #endregion
    }
}