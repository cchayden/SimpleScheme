// <copyright file="EvaluatorOr.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate a sequence of clauses by evaluating each member.
        /// If a value is not #f then return it.  Otherwise return the last value.
        /// </summary>
        private class EvaluatorOr : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorOr class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorOr(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate a sequence of objects, returning the first not #f, or returning the last.
            /// </summary>
            /// <returns>The next step.</returns>
            public override Stepper EvalStep()
            {
                switch (this.Pc)
                {
                    case 0:
                        if (this.Expr == null)
                        {
                            return SubReturn(False);
                        }
                        this.Pc = 1;
                        return SubContinue();

                    case 1:
                        this.Pc = Rest(this.Expr) != null ? 2 : 3;
                        return CallEval(First(this.Expr));

                    case 2:
                        if (Truth(this.ReturnedExpr))
                        {
                            return SubReturn(this.ReturnedExpr);
                        }

                        this.Expr = Rest(this.Expr);
                        this.Pc = 1;
                        return SubContinue();

                    case 3:
                        return SubReturn(this.ReturnedExpr);
                }

                return EvalError("Or: program counter error");
            }
        }
    }
}