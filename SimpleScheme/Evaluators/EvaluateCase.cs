// <copyright file="EvaluateCase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

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
    public sealed class EvaluateCase : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-case";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The list of clauses to test.
        /// </summary>
        private Obj clauses;

        /// <summary>
        /// The evaluated key.
        /// </summary>
        private Obj keyVal;

        /// <summary>
        /// The list of expressions in a matched clause
        /// </summary>
        private Obj exprList;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCase class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateCase(Obj expr, Environment env, Stepper caller, Obj clauses)
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
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateCase(expr, env, caller, List.Rest(expr));
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Steps to evaluate the test.</returns>
        private static Stepper EvalKeyStep(Stepper s)
        {
            return EvaluateExpression.Call(List.First(s.Expr), s.Env, s.ContinueHere(CheckClauseStep));
        }

        /// <summary>
        /// Back here after the key has been evaluated.
        /// Check one of the clauses.
        /// If no clauses left, return empty list.
        /// If clause matches, start evaluating expressions.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The steps to test che clauses.</returns>
        private static Stepper CheckClauseStep(Stepper s)
        {
            EvaluateCase step = (EvaluateCase)s;
            step.keyVal = s.ReturnedExpr;
            while (!EmptyList.Is(step.clauses))
            {
                Obj clause = List.First(step.clauses);
                if (!Pair.Is(clause))
                {
                    return (Stepper)ErrorHandlers.SemanticError("Bad syntax in case: " + clause);
                }

                Obj data = List.First(clause);
                step.exprList = List.Rest(clause);

                // look for else datum
                if (Symbol.Is(data) && Symbol.As(data) == "else")
                {
                    return step.EvalExpr();
                }

                // look for a match within the list of datum items
                while (Pair.Is(data))
                {
                    if (SchemeBoolean.Eqv(step.keyVal, List.First(data)))
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
        /// <returns>The next step to execute.</returns>
        private Stepper EvalExpr()
        {
            if (EmptyList.Is(this.exprList))
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
