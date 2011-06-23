// <copyright file="EvaluateAnd.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence by evaluating each member.
    /// If a value is #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
    public sealed class EvaluateAnd : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-and";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The list of expressions.
        /// </summary>
        private object tests;

        /// <summary>
        /// Initializes a new instance of the EvaluateAnd class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateAnd(Stepper caller, object expr, Environment env)
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
        /// Create an and evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The and evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            // If no expr, avoid creating an evaluator.
            if (expr == null)
            {
                return caller.ContinueStep(SchemeBoolean.True);
            }

            return new EvaluateAnd(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        private Stepper EvalTestStep()
        {
            var nextStep = Rest(this.tests) == null ? (StepperFunction)this.ReturnStep : this.LoopStep;
            return EvaluateExpression.Call(ContinueHere(nextStep), First(this.tests));
        }

        /// <summary>
        /// If the evaluated expression is false, we are done.
        /// Otherwise, step down the list and try again at step 1.
        /// </summary>
        /// <returns>The result, or this step to loop back to previous step.</returns>
        private Stepper LoopStep()
        {
            if (SchemeBoolean.IsFalse(this.ReturnedExpr))
            {
                return ReturnFromStep(SchemeBoolean.False);
            }

            this.tests = Rest(this.tests);
            return ContinueHere(this.EvalTestStep);
        }
    }
}