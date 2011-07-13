﻿// <copyright file="EvaluateTimeBase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics;
    using Obj = System.Object;

    /// <summary>
    /// Base class for EvaluateTime and EvaluateTimeCall.
    /// Evaluate an expression while timing it.
    /// This may evaluate the expression multiple times.
    /// </summary>
    internal abstract class EvaluateTimeBase : Stepper
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
        private int counter;

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
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        protected EvaluateTimeBase(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.startMem = GC.GetTotalMemory(true);
            this.stopwatch = Stopwatch.StartNew();
            ContinueHere(this.InitialStep);
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Start by setting up timers and counter.
        /// </summary>
        /// <returns>Continue to next step.</returns>
        protected Stepper InitialStep()
        {
            Obj y = Second(Expr);
            this.counter = EmptyList.IsType(y) ? 1 : (int)Number.Num(y);
            this.i = 0;
            return ContinueHere(this.Step1);
        }

        /// <summary>
        /// Subclass must provide an implementation.
        /// This evaluates the expression, in a way that is appropriate
        ///   to the function (eval or apply).
        /// </summary>
        /// <returns>The next step.</returns>
        protected abstract Stepper Step1();

        /// <summary>
        /// Back here after expression evaluation.  
        /// Increment and test the counter.
        /// If not done, loop back.
        /// Otherwise, calculate elapsed time and mem use.
        /// </summary>
        /// <returns>Continue, or else give the timer results.</returns>
        protected Stepper Step2()
        {
            this.i++;
            if (this.i < this.counter)
            {
                return this;
            }

            this.stopwatch.Stop();
            long time = this.stopwatch.ElapsedMilliseconds;
            long mem = GC.GetTotalMemory(false) - this.startMem;
            return ReturnFromStep(
                    Cons(
                        ReturnedExpr, 
                        MakeList(MakeList(Number.Num(time), "msec"), MakeList(Number.Num(mem), "bytes"))));
        }
        #endregion
    }
}
