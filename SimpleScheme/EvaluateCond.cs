// <copyright file="EvaluateCond.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Reduce a conditiona;
    /// </summary>
    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)<r4rs>
    //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
    public sealed class EvaluateCond : Stepper
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
            this.Pc = this.InitialStep;
            IncrementCounter("cond");
        }

        /// <summary>
        /// Calls a cond evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The reduce cond evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateCond(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluates a cond.  This step starts by checking for special conditions
        ///   such as else or the end of the list.
        /// Most often it evaluates the first clause.
        /// </summary>
        /// <returns>Usually, the step to evaluate the first clause.</returns>
        private Stepper InitialStep()
        {
            if (Expr == null)
            {
                return ReturnFromStep(SchemeBoolean.False);
            }

            this.clause = List.First(Expr);
            this.LoopStep(List.Rest(Expr));
            if (List.First(this.clause) as string == "else")
            {
                this.result = null;
                this.Pc = this.ReturnStep;
                return this;
            }

            this.Pc = this.EvalClauseStep;
            return EvaluatorMain.Call(this, List.First(this.clause));
        }

        /// <summary>
        /// Come here after evaluating a clause.
        /// If it is true, then proceed to evaluate the consequent.  
        /// Otherwise, go back to initial step.
        /// The list was stepped down already in the previous step.
        /// </summary>
        /// <returns>The next step, either loop or finish.</returns>
        private Stepper EvalClauseStep()
        {
            this.result = ReturnedExpr;
            this.Pc = SchemeBoolean.Truth(this.result) ? (Func<Stepper>)this.ReturnStep : this.InitialStep;
            return this;
        }

        /// <summary>
        /// Come here when we have found a consequent to evaluate.
        /// Handle the varous forms for conequent.
        /// Evaluate and return the consequent.
        /// </summary>
        /// <returns>Execution continues with the caller.</returns>
        private new Stepper ReturnStep()
        {
            object res;
            if (List.Rest(this.clause) == null)
            {
                res = List.MakeList("quote", this.result);
            }
            else if (List.Second(this.clause) as string == "=>")
            {
                res = List.MakeList(List.Third(this.clause), List.MakeList("quote", this.result));
            }
            else
            {
                res = List.Cons("begin", List.Rest(this.clause));
            }

            return EvaluatorMain.Call(this.Parent, res);
        }
    }
}