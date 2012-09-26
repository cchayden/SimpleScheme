// <copyright file="EvaluateTimeBase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Base class for EvaluateTime and EvaluateTimeCall.
    /// Evaluate an expression while timing it.
    /// This may evaluate the expression multiple times.
    /// </summary>
    internal class EvaluateTimeBase : Evaluator
    {
        #region Fields
        /// <summary>
        /// The amount of memory at the start.
        /// </summary>
        private readonly long startMem;

        /// <summary>
        /// The starting time.
        /// </summary>
        private readonly Stopwatch stopwatch;

        /// <summary>
        /// The number of times to repeat the evaluation.
        /// </summary>
        private readonly int counter;

        /// <summary>
        /// How far we are into the repeated evaluation.
        /// </summary>
        private int i;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateTimeBase class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="count">The number of times to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="counterId">The counter id of the evaluator.</param>
        protected EvaluateTimeBase(SchemeObject expr, int count, Environment env, Evaluator caller, int counterId)
            : base(OpCode.Initial, expr, env, caller, counterId)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counterId >= 0);
            this.counter = count;
            this.startMem = GC.GetTotalMemory(true);
            this.stopwatch = Stopwatch.StartNew();
        }
        #endregion

        #region Steps
        /// <summary>
        /// Start by setting up timers and counter.
        /// </summary>
        /// <returns>Continue to next step.</returns>
        protected override Evaluator InitialStep()
        {
            this.i = 0;
            this.Pc = OpCode.Evaluate;
            return this;
        }

        /// <summary>
        /// Back here after expression evaluation.  
        /// Increment and test the counter.
        /// If not done, loop back.
        /// Otherwise, calculate elapsed time and mem use.
        /// </summary>
        /// <returns>Continue, or else give the timer results.</returns>
        protected override Evaluator DoneStep()
        {
            this.i++;
            if (this.i < this.counter)
            {
                this.Pc = OpCode.Evaluate;
                return this;
            }

            this.stopwatch.Stop();
            long time = this.stopwatch.ElapsedMilliseconds;
            long mem = GC.GetTotalMemory(false) - this.startMem;
            Evaluator caller = this.Caller;
            caller.ReturnedExpr = 
                MakeList(
                    this.ReturnedExpr,
                    MakeList((Number)time,  (Symbol)"msec"),
                    MakeList((Number)mem, (Symbol)"bytes"));
            return caller;
        }
        #endregion
    }
}
