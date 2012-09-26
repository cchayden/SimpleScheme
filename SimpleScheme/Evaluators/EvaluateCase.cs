// <copyright file="EvaluateCase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
    public sealed class EvaluateCase : Evaluator
    {
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
            : base(expr, env, caller)
        {
            this.clauses = clauses;
            this.ContinueHere(EvalKeyStep);
            this.IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates a case evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The case evaluator.</returns>
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            return new EvaluateCase(expr, env, caller, Rest(expr));
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Steps to evaluate the test.</returns>
        private static Evaluator EvalKeyStep(Evaluator s)
        {
            return EvaluateExpression.Call(First(s.Expr), s.Env, s.ContinueHere(CheckClauseStep));
        }

        /// <summary>
        /// Back here after the key has been evaluated.
        /// Check one of the clauses.
        /// If no clauses left, return empty list.
        /// If clause matches, start evaluating expressions.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The steps to test che clauses.</returns>
        private static Evaluator CheckClauseStep(Evaluator s)
        {
            var step = (EvaluateCase)s;
            step.keyVal = EnsureSchemeObject(s.ReturnedExpr);
            while (!(step.clauses is EmptyList))
            {
                SchemeObject clause = First(step.clauses);
                if (!(clause is Pair))
                {
                    ErrorHandlers.SemanticError("Bad syntax in case: " + clause);
                    return null;
                }

                SchemeObject data = First(clause);
                step.exprList = Rest(clause);

                // look for else datum
                if (data is Symbol && data.ToString() == "else")
                {
                    return step.EvalExpr();
                }

                // look for a match within the list of datum items
                while (data is Pair)
                {
                    if (SchemeBoolean.Eqv(step.keyVal, First(data)).Value)
                    {
                        return step.EvalExpr();
                    }

                    data = Rest(data);
                }

                // didn't find a match -- look at the next clause
               step.clauses = Rest(step.clauses);
            }

            // no clauses matched -- unspecified
            return s.ReturnUndefined();
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
                return this.ReturnFromStep(this.keyVal);
            }

            // eval and return last expr
            return EvaluateSequence.Call(this.exprList, this.Caller.Env, this.Caller);
        }
        #endregion
    }
}
