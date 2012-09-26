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
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-case";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The list of clauses to test.
        /// </summary>
        private ISchemeObject clauses;

        /// <summary>
        /// The evaluated key.
        /// </summary>
        private ISchemeObject keyVal;

        /// <summary>
        /// The list of expressions in a matched clause
        /// </summary>
        private ISchemeObject exprList;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCase class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="clauses">The case clauses.</param>
        private EvaluateCase(ISchemeObject expr, Environment env, Evaluator caller, ISchemeObject clauses)
            : base(expr, env, caller)
        {
            this.clauses = clauses;
            ContinueHere(EvalKeyStep);
            IncrementCounter(counter);
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
        public static Evaluator Call(ISchemeObject expr, Environment env, Evaluator caller)
        {
            return new EvaluateCase(expr, env, caller, List.Rest(expr));
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
            return EvaluateExpression.Call(List.First(s.Expr), s.Env, s.ContinueHere(CheckClauseStep));
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
            step.keyVal = s.ReturnedExpr;
            while (!(step.clauses is EmptyList))
            {
                ISchemeObject clause = List.First(step.clauses);
                if (!(clause is Pair))
                {
                    return (Evaluator)ErrorHandlers.SemanticError("Bad syntax in case: " + clause);
                }

                ISchemeObject data = List.First(clause);
                step.exprList = List.Rest(clause);

                // look for else datum
                if (data is Symbol && data.ToString() == "else")
                {
                    return step.EvalExpr();
                }

                // look for a match within the list of datum items
                while (data is Pair)
                {
                    if (SchemeBoolean.Eqv(step.keyVal, List.First(data)).Value)
                    {
                        return step.EvalExpr();
                    }

                    data = List.Rest(data);
                }

                // didn't find a match -- look at the next clause
               step.clauses = List.Rest(step.clauses);
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
                return ReturnFromStep(this.keyVal);
            }

            // eval and return last expr
            return EvaluateSequence.Call(this.exprList, this.Caller.Env, this.Caller);
        }
        #endregion
    }
}
