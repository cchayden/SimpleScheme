// <copyright file="EvaluateSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a set! expression.
    /// </summary>
    //// <r4rs section="4.1.6">(set <variable> <expression>)</r4rs>
    public sealed class EvaluateSet : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-set";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Initializes a new instance of the EvaluateSet class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateSet(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
            ContinueHere(this.InitialStep);
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
        /// Calls a set evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The set evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateSet(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluate the second expression (rhs).
        /// </summary>
        /// <returns>Code to evaluate the second expression.</returns>
        private Stepper InitialStep()
        {
            return EvaluateExpression.Call(ContinueHere(this.SetStep), Second(Expr));
        }

        /// <summary>
        /// Back here after evaluation.  Assign the result to the variable in the environment
        ///   named by the first part of the expression.
        /// </summary>
        /// <returns>Returns to caller.</returns>
        private Stepper SetStep()
        {
            return ReturnFromStep(this.Env.Set(First(Expr), ReturnedExpr));
        }
    }
}