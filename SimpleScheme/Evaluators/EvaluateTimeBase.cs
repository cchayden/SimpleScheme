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
    public abstract class EvaluateTimeBase : Evaluator
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
        /// <param name="count">The number of times to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        protected EvaluateTimeBase(SchemeObject expr, int count, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            this.counter = count;
            this.startMem = GC.GetTotalMemory(true);
            this.stopwatch = Stopwatch.StartNew();
            this.ContinueHere(InitialStep);
        }
        #endregion

        #region Protected Static Methods
        /// <summary>
        /// Subclass must provide an implementation.
        /// This evaluates the expression, in a way that is appropriate
        ///   to the function (eval or apply).
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step.</returns>
        protected static Evaluator Step1(Evaluator s)
        {
            return ((EvaluateTimeBase)s).Step1();
        }

        /// <summary>
        /// Back here after expression evaluation.  
        /// Increment and test the counter.
        /// If not done, loop back.
        /// Otherwise, calculate elapsed time and mem use.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continue, or else give the timer results.</returns>
        protected static Evaluator Step2(Evaluator s)
        {
            var step = (EvaluateTimeBase)s;
            step.i++;
            if (step.i < step.counter)
            {
                  return s.ContinueHere(Step1);
            }

            step.stopwatch.Stop();
            long time = step.stopwatch.ElapsedMilliseconds;
            long mem = GC.GetTotalMemory(false) - step.startMem;
            return s.ReturnFromStep(
                MakeList(
                    s.ReturnedExpr,
                    MakeList((Number)time,  (Symbol)"msec"),
                    MakeList((Number)mem, (Symbol)"bytes")));
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Step1 is defined in the derived classes.
        /// </summary>
        /// <returns>The next step to take.</returns>
        protected abstract Evaluator Step1();
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Start by setting up timers and counter.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continue to next step.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateTimeBase)s;
            step.i = 0;
            return s.ContinueHere(Step1);
        }
        #endregion
    }
}
