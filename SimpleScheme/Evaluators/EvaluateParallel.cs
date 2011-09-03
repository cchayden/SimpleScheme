﻿// <copyright file="EvaluateParallel.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a sequence of exprs in parallel by evaluating each member.
    /// Return a list of the results, in the (reverse) order in which they are produced.
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
        /// Used to lock when testing for return.
        /// </summary>
        private readonly object lockObj = new object();

        /// <summary>
        /// Accumulates the return values in a thread-safe way.
        /// </summary>
        private readonly Queue<Tuple<Obj, ReturnType>> returnQueue = new Queue<Tuple<Obj, ReturnType>>();

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
        private Obj accum;
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
            this.accum = EmptyList.Instance;
            ContinueHere(InitialStep);
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
            if (EmptyList.Is(expr))
            {
                return caller.ReturnFromStep(EmptyList.Instance);
            }

            return new EvaluateParallel(expr, env, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Override the copy operation so that we can reset the
        ///   forked and joined counters.
        /// In the copy, once it resumes, none of the suspended operations will come back to it.
        /// But any new ones that it does will, so counting has to start over.
        /// </summary>
        /// <returns>The evaluator copy.</returns>
        public override Evaluator Clone()
        {
            var copy = (EvaluateParallel)this.MemberwiseClone();
            copy.forked = copy.joined = 0;
            return copy;
        }

        /// <summary>
        /// Get a return value from EvaluateExpressionWithCatch, along with a flag.
        /// Make sure it is done within a lock.
        /// Store in a queue rather than in variables because this can be executed
        ///   concurrently in several threads.
        /// </summary>
        /// <param name="exp">The value to save as the returned value.</param>
        /// <param name="flag">The return flag.</param>
        /// <returns>The next evaluator, which is the caller.</returns>
        public override Evaluator UpdateReturnValue(Obj exp, ReturnType flag)
        {
            lock (this.lockObj)
            {
                this.returnQueue.Enqueue(new Tuple<object, ReturnType>(exp, flag));
            }

            return this;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Initial step: evaluate the first expression.
        /// Instead of calling normal EvaluateExpression, call a variant that catches suspended
        ///   execution and halts the evaluation.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            return EvaluateExpressionWithCatch.Call(List.First(s.Expr), s.Env, s.ContinueHere(LoopStep));
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
                        // return after suspension == end or return
                        step.accum = Pair.Cons(step.ReturnedExpr, step.accum);
                        step.joined++;
                        return step.joined >= step.forked ? step.ReturnFromStep(step.accum) : step.ReturnEnded();

                    case ReturnType.SynchronousReturn:
                        step.accum = Pair.Cons(step.ReturnedExpr, step.accum);
                        break;
                }

                s.StepDownExpr();
                if (EmptyList.Is(s.Expr))
                {
                    if (step.joined < step.forked)
                    {
                        return new SuspendedEvaluator(s.ReturnedExpr, s.ContinueHere(LoopStep));
                    }

                    return s.ReturnFromStep(step.accum);
                }

                return EvaluateExpressionWithCatch.Call(List.First(s.Expr), s.Env, s.ContinueHere(LoopStep));
            }
        }

        /// <summary>
        /// Get return value and flag from queue.
        /// This MUST be called within the mutex.
        /// </summary>
        private void FetchReturnValue()
        {
            // Now it is safe to store return value and flag
            Tuple<Obj, ReturnType> ret = this.returnQueue.Dequeue();
            base.UpdateReturnValue(ret.Item1, ret.Item2);
        }
        #endregion
    }
}