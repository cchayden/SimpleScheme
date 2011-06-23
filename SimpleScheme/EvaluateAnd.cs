// <copyright file="EvaluateAnd.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Evaluate a sequence by evaluating each member.
    /// If a value is #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
    public sealed class EvaluateAnd : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateAnd class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateAnd(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.InitialStep;
            IncrementCounter("and");
        }

        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The and evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateAnd(caller, expr, caller.Env);
        }

        /// <summary>
        /// Start by checking the expression to see if it is empty.
        /// </summary>
        /// <returns>Normally, returns this step to go immediately to the next step.</returns>
        private Stepper InitialStep()
        {
            if (this.Expr == null)
            {
                return ReturnFromStep(SchemeBoolean.True);
            }

            this.Pc = this.EvalExprStep;
            return this;
        }

        /// <summary>
        /// Evaluate the next expression in the list.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalExprStep()
        {
            this.Pc = List.Rest(this.Expr) == null ? (Func<Stepper>)this.ReturnStep : this.LoopStep;
            return EvaluatorMain.Call(this, List.First(this.Expr));
        }

        /// <summary>
        /// If the evaluated expression is false, we are done.
        /// Otherwise, step down the list and try again at step 1.
        /// </summary>
        /// <returns>The result, or this step to loop back to previous step.</returns>
        private Stepper LoopStep()
        {
            if (SchemeBoolean.IsFalse(this.ReturnedExpr))
            {
                return ReturnFromStep(SchemeBoolean.False);
            }

            this.Pc = this.EvalExprStep;
            return this.LoopStep(List.Rest(this.Expr));
        }
    }
}