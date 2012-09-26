// <copyright file="EvaluateCase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a case expression.
    /// First evaluate the key.
    /// Match it with each of datum1, datum2, ...
    /// If if matches, or if it is an else clause, then execute the expressions
    ///     and return the last one.
    /// If there are no expressions, return the evaluated key.
    /// Keep going with successive clauses until there is a match.
    /// </summary>
    //// <r4rs section="4.2.1">(case <key> <clause1> <clause2> ...)<r4rs>
    //// <r4rs section="4.2.1">clause: ((<datum1> ...) <expression1> <expression2> ...)<r4rs>
    //// <r4rs section="4.2.1">else clause: (else <expression1> <expression2> ...)<r4rs>
    internal sealed class EvaluateCase : Evaluator
    {
        #region Fields
        /// <summary>
        /// The list of clauses to test.
        /// </summary>
        private SchemeObject clauses;

        /// <summary>
        /// The evaluated key.
        /// </summary>
        private SchemeObject keyVal;

        /// <summary>
        /// The list of expressions in a matched clause
        /// </summary>
        private SchemeObject exprList;
        #endregion

        #region Call
        /// <summary>
        /// Creates a case evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The case evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalKeyStep()
        {
            this.Pc = OpCode.CheckClause;
            return EvaluateExpression.Call(First(this.Expr), this.Env, this);
        }

        /// <summary>
        /// Back here after the key has been evaluated.
        /// Check one of the clauses.
        /// If no clauses left, return empty list.
        /// If clause matches, start evaluating expressions.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator CheckClauseStep()
        {
            this.keyVal = this.ReturnedExpr;
            while (!(this.clauses is EmptyList))
            {
                SchemeObject clause = First(this.clauses);
                if (!(clause is Pair))
                {
                    ErrorHandlers.SemanticError("Bad syntax in case: " + clause);
                }

                SchemeObject data = First(clause);
                this.exprList = Rest(clause);

                // look for else datum
                if (data is Symbol && data.ToString() == "else")
                {
                    return this.EvalExpr();
                }

                // look for a match within the list of datum items
                while (data is Pair)
                {
                    if (SchemeBoolean.Eqv(this.keyVal, First(data)).Value)
                    {
                        return this.EvalExpr();
                    }

                    data = Rest(data);
                }

                // didn't find a match -- look at the next clause
               this.clauses = Rest(this.clauses);
            }

            // no clauses matched -- unspecified
            return this.ReturnFromEvaluator(Undefined.Instance);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateCase class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateCase New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateCase>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateCase class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateCase Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Initialize(OpCode.EvalKey, expr, env, caller);
            this.clauses = Rest(expr);
            return this;
        }
        #endregion

        #region Private Methods

        /// <summary>
        /// Common logic for evaluating expressions in the clause expression list.
        /// </summary>
        /// <returns>The next evaluator to execute.</returns>
        private Evaluator EvalExpr()
        {
            if (this.exprList is EmptyList)
            {
                // if no expressions, return key value
                Contract.Assume(this.keyVal != null);
                return this.ReturnFromEvaluator(this.keyVal);
            }

            // eval and return last expr
            Contract.Assume(this.exprList != null);
            var list = this.exprList;
            var ev = this.Env;
            var c = this.Caller;
            this.Reclaim();
            return EvaluateSequence.Call(list, ev, c);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.degenerate || this.clauses != null);
        }
        #endregion
    }
}
