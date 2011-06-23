// <copyright file="EvaluateOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
    public sealed class EvaluateOr : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateOr class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateOr(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.InitialStep;
            IncrementCounter("or");
        }

        /// <summary>
        /// Calls an or evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The or evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateOr(caller, expr, caller.Env);
        }

        /// <summary>
        /// Start by checking for empty list.
        /// </summary>
        /// <returns>Normally, skips to the next step.</returns>
        private Stepper InitialStep()
        {
            if (this.Expr == null)
            {
                return ReturnFromStep(SchemeBoolean.False);
            }

            this.Pc = this.EvalExprStep;
            return this;
        }

        /// <summary>
        /// Evaluate the next expression in the list.
        /// </summary>
        /// <returns>Steps to evaluate the expression.</returns>
        private Stepper EvalExprStep()
        {
            this.Pc = List.Rest(this.Expr) == null ? (Func<Stepper>)this.ReturnStep : this.LoopStep;
            return EvaluatorMain.Call(this, List.First(this.Expr));
        }

        /// <summary>
        /// If the expression evaluated to true, we are done and we can return the expression value.
        /// Otherwise, step down the list.
        /// </summary>
        /// <returns>The evaluation result, or loops back to evaluate the next item.</returns>
        private Stepper LoopStep()
        {
            if (SchemeBoolean.Truth(this.ReturnedExpr))
            {
                return ReturnFromStep(this.ReturnedExpr);
            }

            this.Pc = this.EvalExprStep;
            return this.LoopStep(List.Rest(this.Expr));
        }
    }
}