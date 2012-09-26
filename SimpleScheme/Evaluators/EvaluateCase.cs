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
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper evalKeyStep = GetStepper("EvalKeyStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper checkClauseStep = GetStepper("CheckClauseStep");

        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-case");

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

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCase class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="clauses">The case clauses.</param>
        private EvaluateCase(SchemeObject expr, Environment env, Evaluator caller, SchemeObject clauses)
            : base(evalKeyStep, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(clauses != null);
            Contract.Requires(counter >= 0);
            this.clauses = clauses;
        }
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
            return new EvaluateCase(expr, env, caller, Rest(expr));
        }
        #endregion

        #region Steps
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <returns>Steps to evaluate the test.</returns>
        protected override Evaluator EvalKeyStep()
        {
            this.Pc = checkClauseStep;
            return EvaluateExpression.Call(First(this.Expr), this.Env, this);
        }

        /// <summary>
        /// Back here after the key has been evaluated.
        /// Check one of the clauses.
        /// If no clauses left, return empty list.
        /// If clause matches, start evaluating expressions.
        /// </summary>
        /// <returns>The steps to test che clauses.</returns>
        protected override Evaluator CheckClauseStep()
        {
            this.keyVal = this.ReturnedExpr;
            while (!(this.clauses is EmptyList))
            {
                SchemeObject clause = First(this.clauses);
                if (!(clause is Pair))
                {
                    ErrorHandlers.SemanticError("Bad syntax in case: " + clause, null);
                    return null;
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
            Evaluator caller = this.Caller;
            caller.ReturnedExpr = Undefined.Instance;
            return caller;
        }

        /// <summary>
        /// Common logic for evaluating expressions in the clause expression list.
        /// </summary>
        /// <returns>The next evaluator to execute.</returns>
        private Evaluator EvalExpr()
        {
            if (this.exprList is EmptyList)
            {
                // if no expressions, return key value
                Evaluator caller = this.Caller;
                Contract.Assume(this.keyVal != null);
                caller.ReturnedExpr = this.keyVal;
                return caller;
            }

            // eval and return last expr
            Contract.Assume(this.exprList != null);
            return EvaluateSequence.Call(this.exprList, this.Env, this.Caller);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.clauses != null);
        }
        #endregion
    }
}
