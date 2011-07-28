// <copyright file="EvaluateAnd.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a sequence by evaluating each member.
    /// If a value is #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
    public sealed class EvaluateAnd : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-and";

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
        /// Initializes a new instance of the EvaluateAnd class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateAnd(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.tests = expr;
            ContinueHere(EvalTestStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The and evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            // If no expr, avoid creating an evaluator.
            if (EmptyList.IsEmptyList(expr))
            {
                return caller.ContinueStep(SchemeBoolean.True);
            }

            return new EvaluateAnd(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step to execute.</returns>
        private static Stepper EvalTestStep(Stepper s)
        {
            EvaluateAnd step = (EvaluateAnd)s;
            if (EmptyList.IsEmptyList(List.Rest(step.tests)))
            {
                // On the last test, return directly to the caller, but use
                //  the current env.  This is to achieve tail recursion.
                return EvaluateExpression.Call(List.First(step.tests), s.Env, s.Caller);
            }

            return EvaluateExpression.Call(List.First(step.tests), s.Env, s.ContinueHere(LoopStep));
        }

        /// <summary>
        /// If the evaluated expression is false, we are done.
        /// Otherwise, step down the list and try again at step 1.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The result, or this step to loop back to previous step.</returns>
        private static Stepper LoopStep(Stepper s)
        {
            EvaluateAnd step = (EvaluateAnd)s;
            if (SchemeBoolean.IsFalse(s.ReturnedExpr))
            {
                return s.ReturnFromStep(SchemeBoolean.False);
            }

            step.tests = List.Rest(step.tests);
            return s.ContinueHere(EvalTestStep);
        }
        #endregion
    }
}