// <copyright file="EvaluateTimeBase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics;

    /// <summary>
    /// Base class for EvaluateTime and EvaluateTimeCall.
    /// Evaluate an expression while timing it.
    /// This may evaluate the expression multiple times.
    /// </summary>
    public abstract class EvaluateTimeBase : Stepper
    {
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

        /// <summary>
        /// Initializes a new instance of the EvaluateTimeBase class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        protected EvaluateTimeBase(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.startMem = GC.GetTotalMemory(true);
            this.stopwatch = Stopwatch.StartNew();
            this.Pc = this.InitialStep;
        }

        /// <summary>
        /// Start by setting up timers and counter.
        /// </summary>
        /// <returns>Continue to next step.</returns>
        protected Stepper InitialStep()
        {
            object y = List.Second(Expr);
            this.counter = y == null ? 1 : (int)Number.Num(y);
            this.i = 0;
            this.Pc = this.Step1;
            return this;
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
                    List.Cons(
                        ReturnedExpr, 
                        List.MakeList(List.MakeList(Number.Num(time), "msec"), List.MakeList(Number.Num(mem), "bytes"))));
        }
    }
}
