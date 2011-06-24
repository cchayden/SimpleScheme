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
    internal sealed class EvaluateCase : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-case";

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
        private EvaluateCase(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(this.EvaluateKeyStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Creates a case evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The case evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateCase(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <returns>Steps to evaluate the test.</returns>
        private Stepper EvaluateKeyStep()
        {
            this.clauses = Rest(Expr);
            return EvaluateExpression.Call(First(Expr), this.Env, ContinueHere(this.AssignKey));
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
            while (this.clauses != List.Empty)
            {
                Obj clause = First(this.clauses);
                if (!(clause is Pair))
                {
                    return ErrorHandlers.EvalError("Case: bad syntax in case: " + clause);
                }

                Obj data = First(clause);
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
            return ReturnUndefined();
        }

        /// <summary>
        /// Common logic for evaluating expressions in the clause expression list.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalExpr()
        {
            if (this.exprList == List.Empty)
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
