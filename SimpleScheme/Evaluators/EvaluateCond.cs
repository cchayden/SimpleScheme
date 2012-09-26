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
            : base(OpCode.EvalClause, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            this.clauses = expr;
            this.test = EmptyList.Instance;
            this.clause = EmptyList.Instance;
        }
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

            return new EvaluateCond(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluates a clause.  This step starts by checking for special conditions
        ///   such as else or the end of the list.
        /// Most often it evaluates the first clause.
        /// </summary>
        /// <returns>Usually, This evaluator the first clause.</returns>
        protected override Evaluator EvalClauseStep()
        {
            this.clause = First(this.clauses);
            var clause1 = First(this.clause);
            if (clause1 is Symbol && clause1.ToString() == "else")
            {
                this.Pc = OpCode.EvalConsequent;
                return this;
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
        /// <returns>The next step, either loop or finish.</returns>
        protected override Evaluator CheckClauseStep()
        {
            this.test = this.ReturnedExpr;
            if (SchemeBoolean.Truth(this.test).Value)
            {
                this.Pc = OpCode.EvalConsequent;
                return this;
            }

            this.clauses = Rest(this.clauses);
            if (this.clauses is EmptyList)
            {
                Evaluator caller = this.Caller;
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            this.Pc = OpCode.EvalClause;
            return this;
        }

        /// <summary>
        /// Come here when we have found a consequent to evaluate.
        /// Handle the varous forms for conequent.
        /// Evaluate and return the consequent.
        /// </summary>
        /// <returns>Execution continues with the caller.</returns>
        protected override Evaluator EvalConsequentStep()
        {
            if (Rest(this.clause) is EmptyList)
            {
                // no consequent: return the test as the result
                Evaluator caller = this.Caller;
                caller.ReturnedExpr = this.test;
                return caller;
            }

            var clause2 = Second(this.clause);
            if (clause2 is Symbol && clause2.ToString() == "=>")
            {
                // send to recipient -- first evaluate recipient
                this.Pc = OpCode.ApplyRecipient;
                return EvaluateExpression.Call(Third(this.clause), this.Env, this);
            }

            // evaluate and return the sequence of expressions directly
            return EvaluateSequence.Call(Rest(this.clause), this.Env, this.Caller);
        }

        /// <summary>
        /// Apply the recipient function to the value of the test.
        /// The recipient must evaluate to a procecure.
        /// </summary>
        /// <returns>The next evaluator to execute (the return).</returns>
        protected override Evaluator ApplyRecipientStep()
        {
            var proc = Procedure.EnsureProcedure(this.ReturnedExpr);
            return proc.Apply(MakeList(this.test), this.Caller, this.Caller);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.test != null);
            Contract.Invariant(this.clauses != null);
            Contract.Invariant(this.clause != null);
        }
        #endregion
    }
}