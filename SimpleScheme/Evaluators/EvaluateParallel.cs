// <copyright file="EvaluateParallel.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
        /// The count of evaluations that have been forked.
        /// A forked evaluation is one that has been suspended due to an async
        ///   operation, was caught, and execution continued in parallel.
        /// With the "join" option, each forked evaluation must be joined before
        /// execution can proceed.
        /// </summary>
        private int forked;

        /// <summary>
        /// The count of joined evaluations.
        /// A joined evaluation is detected when a previously forked operation returns.
        /// This return represents the final return of the evaluation.
        /// With the "join" option, each joined evaluation but the last needs to halt.
        /// The last needs to finish the parallel evaluation and continue.
        /// </summary>
        private int joined;
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
            this.forked = this.joined = 0;
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
            // Don't check for return because the only way to get here is from
            //   LoopStep, and it already checked.
            var step = (EvaluateParallel)s;
            if (EmptyList.Is(s.Expr))
            {
                if (step.joined < step.forked)
                {
                    return new SuspendedEvaluator(s.ContinueHere(JoinStep));
                }

                return s.ReturnFromStep(new Undefined());
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
            var step = (EvaluateParallel)s;
            Evaluator ar = step.CheckForAsyncReturn();
            if (ar != null)
            {
                return ar;
            }

            s.StepDownExpr();
            return s.ContinueHere(EvalExprStep);
        }

        /// <summary>
        /// Wait here while each of the asynchronous actions completed.
        /// Once the last one is done, then continue executing by returning.
        /// We expect that all the normal and catches will be done by now,
        ///   only async returns allowed.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Where to execute next.</returns>
        private static Evaluator JoinStep(Evaluator s)
        {
            var step = (EvaluateParallel)s;
            return step.CheckForAsyncReturn();
        }

        /// <summary>
        /// Check the return flag to see if it is a special async return value.
        /// These values are generated by EvaluateExpressionWithCatch and are used to communicate
        ///   a caught suspend and the subsequent final return.
        /// If it is return after suspended, count as a join and either return ended or
        ///   continue the evaluation.
        /// If it is caught suspension, count as fork.
        /// If this returns null, then processing continues.
        /// Otherwise, the evaluator returns what this function returns.
        /// </summary>
        /// <returns>The next step -- ended or a return.</returns>
        private Evaluator CheckForAsyncReturn()
        {
            switch (this.ReturnFlag)
            {
                case ReturnType.CaughtSuspended:
                    // caught an asynchronous suspension -- go on to the next expr
                    this.forked++;
                    break;
                case ReturnType.AsynchronousReturn:
                    // return after suspension == end or return
                    this.joined++;
                    return this.joined < this.forked ? this.ReturnEnded() : this.ReturnFromStep(new Undefined());
                case ReturnType.SynchronousReturn:
                    break;
            }

            return null;
        }

        #endregion
    }
}