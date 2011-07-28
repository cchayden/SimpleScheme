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
            ContinueHere(EvalTestStep);
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
            if (EmptyList.IsEmptyList(expr))
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
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Steps to evaluate the expression.</returns>
        private static Stepper EvalTestStep(Stepper s)
        {
            EvaluateOr step = (EvaluateOr)s;
            if (EmptyList.IsEmptyList(List.Rest(step.tests)))
            {
                // On the last test, return directly to the caller, but use
                //  the current env.  This is to achieve tail recursion.
                return EvaluateExpression.Call(List.First(step.tests), s.Env, s.Caller);
            }

            return EvaluateExpression.Call(List.First(step.tests), s.Env, s.ContinueHere(LoopStep));
        }

        /// <summary>
        /// If the expression evaluated to true, we are done and we can return the expression value.
        /// Otherwise, step down the list.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The evaluation result, or loops back to evaluate the next item.</returns>
        private static Stepper LoopStep(Stepper s)
        {
            EvaluateOr step = (EvaluateOr)s;
            if (SchemeBoolean.Truth(s.ReturnedExpr))
            {
                return s.ReturnFromStep(s.ReturnedExpr);
            }

            step.tests = List.Rest(step.tests);
            return s.ContinueHere(EvalTestStep);
        }
        #endregion
    }
}