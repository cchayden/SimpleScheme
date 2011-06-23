// <copyright file="EvaluateOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
    public sealed class EvaluateOr : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "or";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The list of expressions.
        /// </summary>
        private object tests;

        /// <summary>
        /// Initializes a new instance of the EvaluateOr class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateOr(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
            this.tests = expr;
            ContinueHere(this.EvalTestStep);
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
        /// Calls an or evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The or evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            // If no expr, avoid creating an evaluator.
            if (expr == null)
            {
                return caller.ContinueStep(SchemeBoolean.False);
            }

            return new EvaluateOr(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <returns>Steps to evaluate the expression.</returns>
        private Stepper EvalTestStep()
        {
            var nextStep = Rest(this.tests) == null ? (StepperFunction)this.ReturnStep : this.LoopStep;
            return EvaluateExpression.Call(ContinueHere(nextStep), First(this.tests));
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

            this.tests = Rest(this.tests);
            return ContinueHere(this.EvalTestStep);
        }
    }
}