// <copyright file="EvaluateTimeCall.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics;

    /// <summary>
    /// Evaluate an application.
    /// This loops until the application returns a value.
    /// </summary>
    public class EvaluateTimeCall : EvaluatorBase
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
        /// Initializes a new instance of the EvaluateTimeCall class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateTimeCall(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.startMem = GC.GetTotalMemory(true);
            this.stopwatch = Stopwatch.StartNew();
        }

        /// <summary>
        /// Create a new timed evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <returns>The timed evaluator.</returns>
        public static EvaluateTimeCall New(object expr, Environment env, Stepper parent)
        {
            return new EvaluateTimeCall(parent, expr, env);
        }

        /// <summary>
        /// Evaluate an application.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            while (true)
            {
                switch (Pc)
                {
                    case PC.Initial:
                        object y = Second(Expr);
                        this.counter = y == null ? 1 : (int)NumberUtils.Num(y);
                        this.i = 0;
                        Pc = PC.Step1;
                        continue;

                    case PC.Step1:
                        object ans = Procedure.Proc(First(Expr)).Apply(this, null);
                        if (ans is Stepper)
                        {
                            return GoToStep((Stepper)ans, PC.Step2);
                        }

                        Pc = PC.Step2;
                        ReturnedExpr = ans;
                        continue;

                    case PC.Step2:
                        this.i++;
                        if (this.i < this.counter)
                        {
                            continue;
                        }

                        this.stopwatch.Stop();
                        long time = this.stopwatch.ElapsedMilliseconds;
                        long mem = GC.GetTotalMemory(false) - this.startMem;
                        return ReturnFromStep(Cons(ReturnedExpr, List(List(NumberUtils.Num(time), "msec"), List(NumberUtils.Num(mem), "bytes"))));
                }

                return EvalError("TimeCall: program counter error");
            }
        }
    }
}
