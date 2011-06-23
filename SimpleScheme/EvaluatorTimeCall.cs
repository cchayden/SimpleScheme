// <copyright file="EvaluatorTimeCall.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics;

    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// The amount of memory at the start.
        /// </summary>
        private long startMem;

        /// <summary>
        /// The starting time.
        /// </summary>
        private Stopwatch stopwatch;

        /// <summary>
        /// The number of times to repeat the evaluation.
        /// </summary>
        private int counter;

        /// <summary>
        /// How far we are into the repeated evaluation.
        /// </summary>
        private int i;

        /// <summary>
        /// Evaluate an application.
        /// This loops until the application returns a value.
        /// </summary>
        private class EvaluatorTimeCall : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorTimeCall class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorTimeCall(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
                startMem = GC.GetTotalMemory(true);
                stopwatch = Stopwatch.StartNew();
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
                            counter = y == null ? 1 : (int)NumberUtils.Num(y);
                            i = 0;
                            Pc = PC.Step1;
                            continue;

                        case PC.Step1:
                            object ans = Procedure.Proc(First(Expr)).Apply(Interp, this, null);
                            if (ans is Stepper)
                            {
                                return GoToStep(PC.Step2, (Stepper)ans);
                            }

                            Pc = PC.Step2;
                            ReturnedExpr = ans;
                            continue;

                        case PC.Step2:
                            i++;
                            if (i < counter)
                            {
                                continue;
                            }

                            stopwatch.Stop();
                            long time = stopwatch.ElapsedMilliseconds;
                            long mem = GC.GetTotalMemory(false) - startMem;
                            return SubReturn(Cons(ReturnedExpr, List(List(NumberUtils.Num(time), "msec"), List(NumberUtils.Num(mem), "bytes"))));
                    }

                    return EvalError("TimeCall: program counter error");
                }
            }
        }
    }
}
