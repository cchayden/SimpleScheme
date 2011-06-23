// <copyright file="EvaluatorReduceCond.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    public partial class Evaluator
    {
        /// <summary>
        /// Reduce a conditiona;
        /// </summary>
        private class EvaluatorReduceCond : Evaluator
        {
            private object result;
            private object clause;

            /// <summary>
            /// Initializes a new instance of the EvaluatorSet class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorReduceCond(Scheme interp, Evaluator parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            { }

            /// <summary>
            /// Handle a cond by iterating down the list of clauses.
            /// The clauses are (guard expression) pairs.
            /// If they are exhausted, return False.
            /// If we find a True guard or an else, then:
            ///     If the clause has no expression, return the guard.
            ///     Otherwise return the expression to be evaluated.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (this.Pc)
                {
                    case 0:
                        if (Expr == null)
                        {
                            return SubReturn(False);
                        }
                        clause = First(Expr);
                        Expr = Rest(Expr);
                        if (First(clause) as string == "else")
                        {
                            result = null;
                            Pc = 2;
                        }
                        else
                        {
                            Pc = 1;
                            return CallEval(First(clause));
                        }
                        return SubContinue();
                    case 1:
                        result = ReturnedExpr;
                        Pc = Truth(result) ? 2 : 0;
                        return SubContinue();
                    case 2:
                        if (Rest(clause) == null)
                        {
                            return SubReturn(List("quote", result));
                        }

                        if (Second(clause) as string == "=>")
                        {
                            return SubReturn(List(Third(clause), List("quote", result)));
                        }

                        return SubReturn(Cons("begin", Rest(clause)));
                }
                throw new Exception("program counter error");
            }
        }
    }
}