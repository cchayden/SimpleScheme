// <copyright file="EvaluateOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
    internal sealed class EvaluateOr : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        internal const string StepperName = "evaluate-or";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The list of expressions.
        /// </summary>
        private Obj tests;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateOr class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateOr(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.tests = expr;
            ContinueHere(this.EvalTestStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Calls an or evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The or evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            // If no expr, avoid creating an evaluator.
            if (TypePrimitives.IsEmptyList(expr))
            {
                return caller.ContinueStep(SchemeBoolean.False);
            }

            return new EvaluateOr(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <returns>Steps to evaluate the expression.</returns>
        private Stepper EvalTestStep()
        {
            if (TypePrimitives.IsEmptyList(List.Rest(this.tests)))
            {
                // On the last test, return directly to the caller, but use
                //  the current env.  This is to achieve tail recursion.
                return EvaluateExpression.Call(List.First(this.tests), this.Env, this.Caller);
            }

            return EvaluateExpression.Call(List.First(this.tests), this.Env, ContinueHere(this.LoopStep));
        }

        /// <summary>
        /// If the expression evaluated to true, we are done and we can return the expression value.
        /// Otherwise, step down the list.
        /// </summary>
        /// <returns>The evaluation result, or loops back to evaluate the next item.</returns>
        private Stepper LoopStep()
        {
            if (SchemeBoolean.Truth(this.ReturnedExpr))
            {
                return ReturnFromStep(this.ReturnedExpr);
            }

            this.tests = List.Rest(this.tests);
            return ContinueHere(this.EvalTestStep);
        }
        #endregion
    }
}