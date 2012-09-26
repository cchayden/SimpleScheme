﻿// <copyright file="EvaluateCond.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Reduce a conditiona;
    /// </summary>
    //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
    public sealed class EvaluateCond : Evaluator
    {
        #region Fields

        /// <summary>
        /// The symbol "cond"
        /// </summary>
        public static readonly Symbol CondSym = "cond";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-cond");

        /// <summary>
        /// The value of the test expr
        /// </summary>
        private SchemeObject test;

        /// <summary>
        /// The list of clauses in the cond
        /// </summary>
        private SchemeObject clauses;

        /// <summary>
        /// The cond clause that is being processed.
        /// </summary>
        private SchemeObject clause;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCond class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateCond(SchemeObject expr, Environment env, Evaluator caller)
            : base(expr, env, caller, counter)
        {
            this.clauses = expr;
            this.ContinueAt(EvalClauseStep);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Calls a cond evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The reduce cond evaluator.</returns>
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            // If no expr, avoid creating an evaluator.
            if (expr is EmptyList)
            {
                return caller.UpdateReturnValue((SchemeBoolean)false);
            }

            return new EvaluateCond(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluates a clause.  This step starts by checking for special conditions
        ///   such as else or the end of the list.
        /// Most often it evaluates the first clause.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Usually, This evaluator the first clause.</returns>
        private static Evaluator EvalClauseStep(Evaluator s)
        {
            var step = (EvaluateCond)s;
            step.clause = First(step.clauses);
            var clause = First(step.clause);
            if (clause is Symbol && clause.ToString() == "else")
            {
                step.test = EmptyList.Instance;
                return s.ContinueAt(EvalConsequentStep);
            }

            return EvaluateExpression.Call(First(step.clause), s.Env, s.ContinueAt(TestClauseStep));
        }

        /// <summary>
        /// Come here after evaluating a clause.
        /// If it is true, then proceed to evaluate the consequent.  
        /// Otherwise, go back to initial step.
        /// The list was stepped down already in the previous step.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step, either loop or finish.</returns>
        private static Evaluator TestClauseStep(Evaluator s)
        {
            var step = (EvaluateCond)s;
            step.test = EnsureSchemeObject(s.ReturnedExpr);
            if (SchemeBoolean.Truth(step.test).Value)
            {
                return s.ContinueAt(EvalConsequentStep);
            }

            step.clauses = Rest(step.clauses);
            if (step.clauses is EmptyList)
            {
                return step.ReturnFromStep(Undefined.Instance);
            }

            return s.ContinueAt(EvalClauseStep);
        }

        /// <summary>
        /// Come here when we have found a consequent to evaluate.
        /// Handle the varous forms for conequent.
        /// Evaluate and return the consequent.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with the caller.</returns>
        private static Evaluator EvalConsequentStep(Evaluator s)
        {
            var step = (EvaluateCond)s;
            if (Rest(step.clause) is EmptyList)
            {
                // no consequent: return the test as the result
                return step.ReturnFromStep(step.test);
            }

            var clause = Second(step.clause);
            if (clause is Symbol && clause.ToString() == "=>")
            {
                // send to recipient -- first evaluate recipient
                return EvaluateExpression.Call(Third(step.clause), s.Env, s.ContinueAt(ApplyRecipientStep));
            }

            // evaluate and return the sequence of expressions directly
            return EvaluateSequence.Call(Rest(step.clause), s.Env, s.Caller);
        }

        /// <summary>
        /// Apply the recipient function to the value of the test.
        /// The recipient must evaluate to a procecure.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator to execute (the return).</returns>
        private static Evaluator ApplyRecipientStep(Evaluator s)
        {
            var step = (EvaluateCond)s;
            return EvaluateProc.CallQuoted(
                Procedure.EnsureProcedure(s.ReturnedExpr), 
                MakeList(step.test), 
                s.Env, 
                s.Caller);
        }
        #endregion
    }
}