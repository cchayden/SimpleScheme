// <copyright file="EvaluateCond.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Reduce a conditiona;
    /// </summary>
    //// <r4rs section="4.2.1">(cond <clause1> <clause2> ... ()</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> <expression>)</r4rs>
    //// <r4rs section="4.2.1">clause: (<test> => <recipient>)</r4rs>
    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)</r4rs>
    internal sealed class EvaluateCond : Evaluator
    {
        #region Fields
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

        #region Call
        /// <summary>
        /// Calls a cond evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The reduce cond evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);

            // If no expr, avoid creating an evaluator.
            if (expr is EmptyList)
            {
                caller.ReturnedExpr = (SchemeBoolean)false;
                return caller;
            }

            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluates a clause.  This step starts by checking for special conditions
        ///   such as else or the end of the list.
        /// Most often it evaluates the first clause.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalClauseStep()
        {
            this.clause = First(this.clauses);
            var clause1 = First(this.clause);
            if (clause1 is Symbol && clause1.ToString() == "else")
            {
                // now EvaluateConsequent
                return this.EvalConsequentStep();
            }

            this.Pc = OpCode.CheckClause;
            return EvaluateExpression.Call(First(this.clause), this.Env, this);
        }

        /// <summary>
        /// Come here after evaluating a clause.
        /// If it is true, then proceed to evaluate the consequent.  
        /// Otherwise, go back to initial step.
        /// The list was stepped down already in the previous step.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator CheckClauseStep()
        {
            this.test = this.ReturnedExpr;
            if (SchemeBoolean.Truth(this.test).Value)
            {
                // now do EvalConsequent step
                return this.EvalConsequentStep();
            }

            this.clauses = Rest(this.clauses);
            if (this.clauses is EmptyList)
            {
                return this.ReturnFromEvaluator(Undefined.Instance);
            }

            // now do EvalClause
            return this.EvalClauseStep();
        }

        /// <summary>
        /// Come here when we have found a consequent to evaluate.
        /// Handle the varous forms for conequent.
        /// Evaluate and return the consequent.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalConsequentStep()
        {
            if (Rest(this.clause) is EmptyList)
            {
                // no consequent: return the test as the result
                return this.ReturnFromEvaluator(this.test);
            }

            var clause2 = Second(this.clause);
            if (clause2 is Symbol && clause2.ToString() == "=>")
            {
                // send to recipient -- first evaluate recipient
                this.Pc = OpCode.ApplyRecipient;
                return EvaluateExpression.Call(Third(this.clause), this.Env, this);
            }

            // evaluate and return the sequence of expressions directly
            var r = Rest(this.clause);
            var ev = this.Env;
            var c = this.Caller;
            this.Reclaim();
            return EvaluateSequence.Call(r, ev, c);
        }

        /// <summary>
        /// Apply the recipient function to the value of the test.
        /// The recipient must evaluate to a procecure.
        /// </summary>
        /// <returns>The next evaluator.</returns>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator ApplyRecipientStep()
        {
            var proc = Procedure.EnsureProcedure(this.ReturnedExpr);
            SchemeObject list = MakeList(this.test);
            Evaluator c = this.Caller;
            this.Reclaim();
            return proc.Apply(list, c);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateCond class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateCond New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateCond>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateCond class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Newly initialized evaluator.</returns>
        private EvaluateCond Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Initialize(OpCode.EvalClause, expr, env, caller);
            this.clauses = expr;
            this.test = EmptyList.Instance;
            this.clause = EmptyList.Instance;
            return this;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.degenerate || this.test != null);
            Contract.Invariant(this.degenerate || this.clauses != null);
            Contract.Invariant(this.degenerate || this.clause != null);
        }
        #endregion
    }
}