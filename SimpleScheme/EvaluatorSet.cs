// <copyright file="EvaluatorSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        private class EvaluatorSet : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorSet class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorSet(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate a set! expression.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper RunStep()
            {
                switch (Pc)
                {
                    case PC.Initial:
                        Pc = PC.Step1;
                        return CallEval(Second(this.Expr));

                    case PC.Step1:
                        return SubReturn(this.Env.Set(First(this.Expr), ReturnedExpr));
                }

                return EvalError("Set: program counter error");
            }
        }
    }
}