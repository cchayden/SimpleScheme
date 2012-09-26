// <copyright file="EvaluateParallel.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;

    /// <summary>
    /// Evaluate a sequence of exprs in parallel by evaluating each member.
    /// Return a list of the results, in the (reverse) order in which they are produced.
    /// </summary>
    internal sealed class EvaluateParallel : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-parallel");

        /// <summary>
        /// Used to lock when testing for return.
        /// </summary>
        private readonly object lockObj = new object();

        /// <summary>
        /// Accumulates the return values in a thread-safe way.
        /// </summary>
        private readonly Queue<Tuple<SchemeObject, ReturnType>> returnQueue = new Queue<Tuple<SchemeObject, ReturnType>>();

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

        /// <summary>
        /// Accumulate result here.
        /// These are returned as the final result.
        /// The results are accumulated as they are produced, and will not necessarily be 
        ///   deterministic.
        /// </summary>
        private SchemeObject accum;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateParallel class.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateParallel(SchemeObject expr, Environment env, Evaluator caller)
            : base(InitialStep, expr, env, caller, counter)
        {
            this.forked = this.joined = 0;
            this.accum = EmptyList.Instance;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call the parallel evaluator.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The parallel evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            if (expr is EmptyList)
            {
                caller = caller.Caller;
                caller.ReturnedExpr = EmptyList.Instance;
                return caller;
            }

            return new EvaluateParallel(expr, env, caller);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Override the copy operation so that we can reset the
        ///   forked and joined counters.
        /// In the copy, once it resumes, none of the suspended operations will come back to it.
        /// But any new ones that it does will, so counting has to start over.
        /// </summary>
        /// <returns>The evaluator copy.</returns>
        internal override Evaluator Clone()
        {
            lock (this.lockObj)
            {
                var copy = (EvaluateParallel)this.MemberwiseClone();
                copy.forked = copy.joined = 0;
                return copy;
            }
        }

        /// <summary>
        /// Get a return type from EvaluateExpressionWithCatch, and also grab the current returnedExpr.
        /// Make sure it is done within a lock.
        /// Store in a queue rather than in variables because this can be executed
        ///   concurrently in several threads.
        /// </summary>
        /// <param name="flag">The return flag.</param>
        internal override void UpdateReturnFlag(ReturnType flag)
        {
            lock (this.lockObj)
            {
                this.returnQueue.Enqueue(new Tuple<SchemeObject, ReturnType>(this.ReturnedExpr, flag));
            }
        }

        /// <summary>
        /// Get return value and flag from queue.
        /// This MUST be called within the mutex.
        /// </summary>
        private void FetchReturnValue()
        {
            // Now it is safe to store return value and flag
            Tuple<SchemeObject, ReturnType> ret = this.returnQueue.Dequeue();
            this.ReturnedExpr = ret.Item1;
            base.UpdateReturnFlag(ret.Item2);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Initial step: evaluate the first expression.
        /// Instead of calling normal EvaluateExpression, call a variant that catches suspended
        ///   execution and halts the evaluation.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            s.Pc = LoopStep;
            return EvaluateExpressionWithCatch.Call(First(s.Expr), s.Env, s);
        }

        /// <summary>
        /// Comes back here after evaluation completes synchronously or is suspended.
        /// If evaluation is suspended, then EvaluateExpressionWithCatch will catch and return undefined.
        /// Loop back and evaluate another expression.
        /// Continue looping until all evaluations return an actual result.
        /// Accumulate the results and return when everything finishes.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Immediately steps back.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            var step = (EvaluateParallel)s;
            lock (step.lockObj)
            {
                step.FetchReturnValue();
                switch (step.ReturnFlag)
                {
                    case ReturnType.CaughtSuspended:
                        // caught an asynchronous suspension -- go on to the next expr
                        step.forked++;
                        break;
                    case ReturnType.AsynchronousReturn:
                        // return after suspension
                        // Record return result and either end the thread or 
                        //  return from the whole thing.
                        step.accum = Cons(step.ReturnedExpr, step.accum);
                        step.joined++;
                        if (step.joined < step.forked || !(s.Expr is EmptyList))
                        {
                            step.ReturnedExpr = Undefined.Instance;
                            return null;
                        }

                        Evaluator caller = step.Caller;
                        caller.ReturnedExpr = step.accum;
                        return caller;

                    case ReturnType.SynchronousReturn:
                        // synchronous return
                        step.accum = Cons(step.ReturnedExpr, step.accum);
                        break;
                }

                s.Expr = Rest(s.Expr);
                if (s.Expr is EmptyList)
                {
                    // finished with expressions -- either suspend (if there is more pending) or
                    //   return from the whole thing
                    if (step.joined < step.forked)
                    {
                        s.Pc = LoopStep;
                        return new SuspendedEvaluator(s.ReturnedExpr, s);
                    }

                    Evaluator caller = step.Caller;
                    caller.ReturnedExpr = step.accum;
                    return caller;
                }

                s.Pc = LoopStep;
                return EvaluateExpressionWithCatch.Call(First(s.Expr), s.Env, s);
            }
        }
        #endregion
    }
}