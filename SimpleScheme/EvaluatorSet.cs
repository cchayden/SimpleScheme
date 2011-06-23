﻿// <copyright file="EvaluatorSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a set! expression.
    /// </summary>
    public class EvaluatorSet : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorSet class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluatorSet(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Creates a set evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <returns>The set evaluator.</returns>
        public static EvaluatorSet New(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorSet(parent, expr, env);
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