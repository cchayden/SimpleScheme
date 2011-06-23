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
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "case";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

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
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateCase(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
            ContinueHere(this.EvaluateKeyStep);
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
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
            this.clauses = Rest(Expr);
            return EvaluateExpression.Call(ContinueHere(this.AssignKey), First(Expr));
        }

        /// <summary>
        /// Grab the evaluated key and go to the test loop.
        /// </summary>
        /// <returns>The next step: check clause.</returns>
        private Stepper AssignKey()
        {
            this.keyVal = ReturnedExpr;
            return ContinueHere(this.CheckClauseStep);
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
                object clause = First(this.clauses);
                if (!(clause is Pair))
                {
                    return ErrorHandlers.EvalError("Case: bad syntax in case");
                }

                object data = First(clause);
                this.exprList = Rest(clause);

                // look for else datum
                if (data is string && (string)data == "else")
                {
                    return this.EvalExpr();
                }

                // look for a match within the list of datum items
                while (data is Pair)
                {
                    if (SchemeBoolean.Eqv(this.keyVal, First(data)))
                    {
                        return this.EvalExpr();
                    }

                    data = Rest(data);
                }

                // didn't find a match -- look at the next clause
               this.clauses = Rest(this.clauses);
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
            return EvaluateSequence.Call(this.Caller, this.exprList);
        }
    }
}
