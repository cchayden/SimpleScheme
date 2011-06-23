﻿// <copyright file="EvaluateCond.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Reduce a conditiona;
    /// </summary>
    public class EvaluateCond : Stepper
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
        /// Initializes a new instance of the EvaluateCond class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateCond(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Creates a reduce cond evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <returns>The reduce cond evaluator.</returns>
        public static EvaluateCond New(Stepper parent, object expr, Environment env)
        {
            return new EvaluateCond(parent, expr, env);
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
        public override Stepper RunStep()
        {
            while (true)
            {
                switch (this.Pc)
                {
                    case PC.Initial:
                        if (Expr == null)
                        {
                            return ReturnFromStep(False);
                        }

                        this.clause = First(Expr);
                        Expr = Rest(Expr);
                        if (First(this.clause) as string == "else")
                        {
                            this.result = null;
                            Pc = PC.Step2;
                        }
                        else
                        {
                            Pc = PC.Step1;
                            return CallEval(First(clause));
                        }

                        continue;

                    case PC.Step1:
                        this.result = ReturnedExpr;
                        Pc = Truth(this.result) ? PC.Step2 : PC.Initial;
                        continue;

                    case PC.Step2:
                        if (Rest(this.clause) == null)
                        {
                            return ReturnFromStep(List("quote", this.result));
                        }

                        if (Second(this.clause) as string == "=>")
                        {
                            return ReturnFromStep(List(Third(this.clause), List("quote", this.result)));
                        }

                        return ReturnFromStep(Cons("begin", Rest(this.clause)));
                }

                return EvalError("ReduceCond: program counter error");
            }
        }
    }
}