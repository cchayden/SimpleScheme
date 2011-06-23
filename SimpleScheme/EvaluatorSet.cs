// <copyright file="EvaluatorSet.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    public partial class Evaluator
    {
        private class EvaluatorSet : Evaluator
        {
            private object first;

            /// <summary>
            /// Initializes a new instance of the EvaluatorSet class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorSet(Scheme interp, Evaluator parent, object expr, Environment env)
                : base(interp, parent, expr, env) { }

            /// <summary>
            /// Evaluate a set! expression.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (Pc)
                {
                    case 0:
                        Pc = 1;
                        first = First(this.Expr);
                        return CallEval(Second(this.Expr));
                    case 1:
                        this.RetExpr = this.Env.Set(first, ReturnedExpr);
                        break;
                }
                return EvalReturn();
            }

        }
    }
}