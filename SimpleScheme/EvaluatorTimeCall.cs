// <copyright file="EvaluatorTimeCall.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
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
        private long startMem;
        private Stopwatch stopwatch;
        private object ans;
        private int counter;
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
                ans = False;
            }

            /// <summary>
            /// Evaluate an application.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper EvalStep()
            {
                switch (Pc)
                {
                    case 0:
                        object y = Second(Expr);
                        counter = y == null ? 1 : (int) Num(y);
                        i = 0;
                        Pc = 1;
                        return SubContinue();

                    case 1:
                        Pc = 2;
                        object ans = Procedure.Proc(First(Expr)).Apply(Interp, this, null);
                        if (ans is Stepper)
                        {
                            return SubCall((Stepper) ans);
                        }
                        return SubContinue(ans);

                    case 2:
                        i++;
                        if (i < counter)
                        {
                            return SubContinue();
                        }

                        stopwatch.Stop();
                        long time = stopwatch.ElapsedMilliseconds;
                        long mem = GC.GetTotalMemory(false) - startMem;
                        return SubReturn(Cons(ReturnedExpr, List(List(Num(time), "msec"), List(Num(mem), "bytes"))));
                }

                return EvalError("TimeCall: program counter error");
            }
        }
    }
}
