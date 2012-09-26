// <copyright file="EvaluateParallel.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;

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
            : base(OpCode.Initial, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
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
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);

            if (expr is EmptyList)
            {
                caller = caller.Caller;
                Contract.Assert(caller != null);
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
        #endregion

        #region Steps
        /// <summary>
        /// Initial step: evaluate the first expression.
        /// Instead of calling normal EvaluateExpression, call a variant that catches suspended
        ///   execution and halts the evaluation.
        /// </summary>
        /// <returns>The next evaluator.</returns>
        protected override Evaluator InitialStep()
        {
            this.Pc = OpCode.Loop;
            return EvaluateExpressionWithCatch.Call(First(this.Expr), this.Env, this);
        }

        /// <summary>
        /// Comes back here after evaluation completes synchronously or is suspended.
        /// If evaluation is suspended, then EvaluateExpressionWithCatch will catch and return undefined.
        /// Loop back and evaluate another expression.
        /// Continue looping until all evaluations return an actual result.
        /// Accumulate the results and return when everything finishes.
        /// </summary>
        /// <returns>Immediately steps back.</returns>
        protected override Evaluator LoopStep()
        {
            lock (this.lockObj)
            {
                this.FetchReturnValue();
                switch (this.ReturnFlag)
                {
                    case ReturnType.CaughtSuspended:
                        // caught an asynchronous suspension -- go on to the next expr
                        this.forked++;
                        break;
                    case ReturnType.AsynchronousReturn:
                        // return after suspension
                        // Record return result and either end the thread or 
                        //  return from the whole thing.
                        Contract.Assume(this.accum != null);
                        this.accum = Cons(this.ReturnedExpr, this.accum);
                        this.joined++;
                        if (this.joined < this.forked || !(this.Expr is EmptyList))
                        {
                            return new FinalEvaluator(Undefined.Instance);
                        }

                        Evaluator caller = this.Caller;
                        caller.ReturnedExpr = this.accum;
                        return caller;

                    case ReturnType.SynchronousReturn:
                        // synchronous return
                        Contract.Assume(this.accum != null);
                        this.accum = Cons(this.ReturnedExpr, this.accum);
                        break;
                }

                this.Expr = Rest(this.Expr);
                if (this.Expr is EmptyList)
                {
                    // finished with expressions -- either suspend (if there is more pending) or
                    //   return from the whole thing
                    if (this.joined < this.forked)
                    {
                        this.Pc = OpCode.Loop;
                        return new SuspendedEvaluator(this.ReturnedExpr, this.Env, this);
                    }

                    Evaluator caller = this.Caller;
                    Contract.Assume(this.accum != null);
                    caller.ReturnedExpr = this.accum;
                    return caller;
                }

                this.Pc = OpCode.Loop;
                return EvaluateExpressionWithCatch.Call(First(this.Expr), this.Env, this);
            }
        }
        #endregion

        #region PrivateMethods
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

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.returnQueue != null);
        }
        #endregion
    }
}