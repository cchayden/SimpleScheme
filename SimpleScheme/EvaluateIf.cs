// <copyright file="EvaluateIf.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate an if expression.
    /// Evaluate the first part, then depending on its truth value, either
    ///   evaluate the second or third part.
    /// </summary>
    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
    public sealed class EvaluateIf : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-if";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Initializes a new instance of the EvaluateIf class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateIf(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
            ContinueHere(this.EvaluateTestStep);
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
        /// Creates an if evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The if evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateIf(caller, expr, caller.Env);
        }

        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <returns>Steps to evaluate the test.</returns>
        private Stepper EvaluateTestStep()
        {
            return EvaluateExpression.Call(ContinueHere(this.EvaluateAlternativeStep), First(Expr));
        }

        /// <summary>
        /// Back here after the test has been evaluated.
        /// Evaluate and return either the second or third expression.
        /// If there is no thid, the empty list will be evaluated, which is OK.
        /// </summary>
        /// <returns>Execution continues with the caller.</returns>
        private Stepper EvaluateAlternativeStep()
        {
            return EvaluateExpression.Call(
                this.Caller, 
                SchemeBoolean.Truth(ReturnedExpr) ? Second(Expr) : Third(Expr));
        }
    }
}
