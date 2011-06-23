// <copyright file="EvaluateExpandMacro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Expand a macro.
    /// </summary>
    public sealed class EvaluateExpandMacro : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-expand-macro";

        /// <summary>
        /// The macro to expand.
        /// </summary>
        private readonly Macro fn;

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Initializes a new instance of the EvaluateExpandMacro class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The macro to expand.</param>
        private EvaluateExpandMacro(Stepper caller, object expr, Environment env, Macro fn)
            : base(caller, expr, env)
        {
            this.fn = fn;
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
        /// Call an expand evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="fn">The macro to expand.</param>
        /// <returns>The expand evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr, Macro fn)
        {
            return new EvaluateExpandMacro(caller, expr, caller.Env, fn);
        }

        /// <summary>
        /// Apply the macro to the expression.  
        /// </summary>
        /// <returns>The first step to evaluate to macro.</returns>
        private Stepper InitialStep()
        {
            return this.fn.Apply(ContinueHere(this.ExpandStep), Expr);
        }

        /// <summary>
        /// Back here after macro is expanded.  Evaluate the result.
        /// </summary>
        /// <returns>The expanded macro.</returns>
        private Stepper ExpandStep()
        {
            return EvaluateExpression.Call(ContinueReturn(), ReturnedExpr);
        }
    }
}