// <copyright file="EvaluatorClosure.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    public partial class Evaluator
    {
        /// <summary>
        /// Evaluate a closure
        /// </summary>
        private class EvaluatorClosure : Evaluator
        {
            private readonly Closure f;

            /// <summary>
            /// Initializes a new instance of the EvaluatorSet class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            /// <param name="f">The closure to evaluate</param>
            public EvaluatorClosure(Scheme interp, Evaluator parent, object expr, Environment env, Closure f)
                : base(interp, parent, expr, env)
            {
                this.f = f;
            }

            /// <summary>
            /// Evaluate a set! expression.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (this.Pc)
                {
                    case 0:
                        Pc = 1;
                        return CallList(Expr);
                    case 1:
                        this.RetEnv = new Environment(f.Parms, ReturnedExpr, f.Env);
                        return SubReturn(f.Body);
                }
                throw new Exception("program counter error");
            }
        }
    }
}