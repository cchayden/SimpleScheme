// <copyright file="EvaluatorIf.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluator contains all the individual evaluators
    /// </summary>
    public partial class Evaluator
    {
        /// <summary>
        /// Evaluate an if expression.
        /// Evaluate the first part, then depending on its truth value, either
        ///   evaluate the second or third part.
        /// </summary>
        private class EvaluatorIf : Evaluator
        {
            /// <summary>
            /// Initializes a new instance of the EvaluatorIf class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorIf(Scheme interp, Evaluator parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate an if expression.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (Pc)
                {
                    case 0:
                        Pc = 1;
                        return CallEval(First(this.Expr));
                    case 1:
                        return SubReturn(Truth(ReturnedExpr) ? Second(this.Expr) : Third(this.Expr));
                }

                return EvalError("If: program counter error");
            }
        }
    }
}