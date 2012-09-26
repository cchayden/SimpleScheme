﻿// <copyright file="EvaluateCond.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

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
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-cond";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The value of the test expr
        /// </summary>
        private Obj test;

        /// <summary>
        /// The list of clauses in the cond
        /// </summary>
        private Obj clauses;

        /// <summary>
        /// The cond clause that is being processed.
        /// </summary>
        private Obj clause;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCond class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateCond(Obj expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            this.clauses = expr;
            ContinueHere(EvalClauseStep);
            IncrementCounter(counter);
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
        public static Evaluator Call(Obj expr, Environment env, Evaluator caller)
        {
            // If no expr, avoid creating an evaluator.
            if (expr.IsEmptyList())
            {
                return caller.UpdateReturnValue(SchemeBoolean.False);
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
            step.clause = step.clauses.First();
            var clause = step.clause.First();
            if (clause.IsSymbol() && clause.ToString() == "else")
            {
                step.test = EmptyList.New();
                return s.ContinueHere(EvalConsequentStep);
            }

            return EvaluateExpression.Call(step.clause.First(), s.Env, s.ContinueHere(TestClauseStep));
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
            step.test = s.ReturnedExpr;
            if (SchemeBoolean.Truth(step.test).Value)
            {
                return s.ContinueHere(EvalConsequentStep);
            }

            step.clauses = step.clauses.Rest();
            if (step.clauses.IsEmptyList())
            {
                return s.ReturnUndefined();
            }

            return s.ContinueHere(EvalClauseStep);
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
            if (step.clause.Rest().IsEmptyList())
            {
                // no consequent: return the test as the result
                return s.ReturnFromStep(step.test);
            }

            var clause = step.clause.Second();
            if (clause.IsSymbol() && clause.ToString() == "=>")
            {
                // send to recipient -- first evaluate recipient
                return EvaluateExpression.Call(step.clause.Third(), s.Env, s.ContinueHere(ApplyRecipientStep));
            }

            // evaluate and return the sequence of expressions directly
            return EvaluateSequence.Call(step.clause.Rest(), s.Env, s.Caller);
        }

        /// <summary>
        /// Apply the recipient function to the value of the test
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator to execute (the return).</returns>
        private static Evaluator ApplyRecipientStep(Evaluator s)
        {
            var step = (EvaluateCond)s;
            return EvaluateProc.CallQuoted(s.ReturnedExpr.AsProcedure(), step.test.MakeList(), s.Env, s.Caller);
        }
        #endregion
    }
}