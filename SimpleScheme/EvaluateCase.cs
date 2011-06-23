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
    public sealed class EvaluateCase : Stepper
    {
        /// <summary>
        /// The list of clauses to test.
        /// </summary>
        private object clauses;

        /// <summary>
        /// The evaluated key.
        /// </summary>
        private object keyVal;

        /// <summary>
        /// The list of expressions in a matched clause
        /// </summary>
        private object exprList;

        /// <summary>
        /// Initializes a new instance of the EvaluateCase class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateCase(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.EvaluateKeyStep;
            IncrementCounter("case");
        }

        /// <summary>
        /// Creates a case evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The if evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateCase(caller, expr, caller.Env);
        }

        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <returns>Steps to evaluate the test.</returns>
        private Stepper EvaluateKeyStep()
        {
            this.Pc = this.AssignKey;
            this.clauses = List.Rest(this.Expr);
            return EvaluatorMain.Call(this, List.First(this.Expr));
        }

        /// <summary>
        /// Grab the evaluated key and go to the test loop.
        /// </summary>
        /// <returns></returns>
        private Stepper AssignKey()
        {
            this.keyVal = ReturnedExpr;
            this.Pc = this.CheckClauseStep;
            return this;
        }

        /// <summary>
        /// Back here after the key has been evaluated.
        /// Check one of the clauses.
        /// If no clauses left, return empty list.
        /// If clause matches, start evaluating expressions.
        /// </summary>
        /// <returns>The steps to test che clauses.</returns>
        private Stepper CheckClauseStep()
        {
            while (this.clauses != null)
            {
                object clause = List.First(this.clauses);
                if (!(clause is Pair))
                {
                    return ErrorHandlers.EvalError("Case: bad syntax in case");
                }

                object data = List.First(clause);
                this.exprList = List.Rest(clause);

                // look for else datum
                if (data is string && (string)data == "else")
                {
                    return this.EvalExpr();
                }

                // look for a match within the list of datum items
                while (data is Pair)
                {
                    if (SchemeBoolean.Eqv(this.keyVal, List.First(data)))
                    {
                        return this.EvalExpr();
                    }

                    data = List.Rest(data);
                }

                // didn't find a match -- look at the next clause
               this.clauses = List.Rest(this.clauses);
            }

            // no clauses matched -- unspecified
            return ReturnFromStep(null);
        }

        /// <summary>
        /// Common logic for evaluating expressions in the clause expression list.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalExpr()
        {
            if (this.exprList == null)
            {
                // if no expressions, return key value
                return ReturnFromStep(this.keyVal);
            }

            // eval and return last expr
            return EvaluateSequence.Call(this.Parent, this.exprList);
        }
    }
}
