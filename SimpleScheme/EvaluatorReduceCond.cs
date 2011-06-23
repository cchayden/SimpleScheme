// <copyright file="EvaluatorReduceCond.cs" company="Charles Hayden">
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
        /// Reduce a conditiona;
        /// </summary>
        private class EvaluatorReduceCond : Stepper
        {
            /// <summary>
            /// The result to be returned.
            /// </summary>
            private object result;

            /// <summary>
            /// The cond clause that is being processed.
            /// </summary>
            private object clause;

            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorCond class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorReduceCond(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Handle a cond by iterating down the list of clauses.
            /// The clauses are (guard expression) pairs.
            /// If they are exhausted, return False.
            /// If we find a True guard or an else, then:
            ///     If the clause has no expression, return the guard.
            ///     Otherwise return the expression to be evaluated.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper EvalStep()
            {
                switch (this.Pc)
                {
                    case 0:
                        if (Expr == null)
                        {
                            return SubReturn(False);
                        }

                        this.clause = First(Expr);
                        Expr = Rest(Expr);
                        if (First(this.clause) as string == "else")
                        {
                            this.result = null;
                            Pc = 2;
                        }
                        else
                        {
                            Pc = 1;
                            return CallEval(First(clause));
                        }

                        return SubContinue();

                    case 1:
                        this.result = ReturnedExpr;
                        Pc = Truth(this.result) ? 2 : 0;
                        return SubContinue();

                    case 2:
                        if (Rest(this.clause) == null)
                        {
                            return SubReturn(List("quote", this.result));
                        }

                        if (Second(this.clause) as string == "=>")
                        {
                            return SubReturn(List(Third(this.clause), List("quote", this.result)));
                        }

                        return SubReturn(Cons("begin", Rest(this.clause)));
                }

                return EvalError("ReduceCond: program counter error");
            }
        }
    }
}