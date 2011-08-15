#define OLDx
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
        /// If true, then wait for all async operations to complete.
        /// </summary>
        private readonly bool join;

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
        /// <param name="join">If true, join after the exprs have been evaluated.</param>
        private EvaluateParallel(Obj expr, Environment env, Evaluator caller, bool join)
            : base(expr, env, caller)
        {
            this.forked = this.joined = 0;
            this.join = join;
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
        /// <param name="join">If true, join after the exprs have been evaluated.</param>
        /// <returns>The parallel evaluator.</returns>
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller, bool join)
        {
            return new EvaluateParallel(expr, env, caller, join);
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
            EvaluateParallel step = (EvaluateParallel)s;
            if (EmptyList.Is(s.Expr))
            {
                if (step.join)
                {
                    return NewSuspendedEvaluator(null, s.Env, s.ContinueHere(JoinStep));
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
            int parm = Undefined.As(s.ReturnedExpr).Value;
            EvaluateParallel step = (EvaluateParallel)s;
            switch (parm)
            {
                default:
                    // normal return -- not asynchronous
                    s.UpdateExpr(List.Rest(s.Expr));
                    return s.ContinueHere(EvalExprStep);
                case (int)EvaluateExpressionWithCatch.CatchCode.CaughtSuspended:
                    // caught an asynchronous suspension
                    step.forked++;
                    s.UpdateExpr(List.Rest(s.Expr));
                    return s.ContinueHere(EvalExprStep);
                case (int)EvaluateExpressionWithCatch.CatchCode.ReturnAfterSuspended:
                    // return from previously suspended evaluation
                    step.joined++;
                    if (step.joined < step.forked)
                    {
                        return s.Interp.Halted;
                    }

                    return s.ReturnFromStep(new Undefined());
            }
        }

        /// <summary>
        /// Wait here while each of the asynchronous actions completed.
        /// Once the last one is done, then continue executing by returning.
        /// We expect that all the normal and catches will be done by now,
        ///   only async returns allowed.
/// TODO what happens if a parallel expression contains an async call, a long sleep, and another async operation?
/// The first will be caught, then execution will proceed and we will get into join.
/// Then the second will be caught, and we need to continue at that point.
/// So we cannot have a join step.
/// TODO what happens if a parallel expression contains two async calls?
/// TODO can we get a catch or a return after catch in EvalExprStep?
/// The catch must come after eval -- so after eval it is either finish or catch.  
/// But the return after catch can come in any state.
/// TODO what if the last clause suspends, then resumes and suspends again?
/// How do we know when the last one is done?
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Where to execute next.</returns>
        private static Evaluator JoinStep(Evaluator s)
        {
            int parm = Undefined.As(s.ReturnedExpr).Value;
            EvaluateParallel step = (EvaluateParallel)s;
            switch (parm)
            {
                default:
                    ErrorHandlers.InternalError("Unexpected value in join " + parm);
                    return null;
                case (int)EvaluateExpressionWithCatch.CatchCode.ReturnAfterSuspended:
                    {
                    step.joined++;
                    if (step.joined < step.forked)
                        {
                            return s.Interp.Halted;
                        }

                        return s.ReturnFromStep(new Undefined());
                    }
            }
        }

        #endregion
    }
}