// <copyright file="EvaluateProcQuoted.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Apply a proc to args without evaluation.
    /// </summary>
    public sealed class EvaluateProcQuoted : EvaluateProc
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-proc-quoted";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Initializes a new instance of the EvaluateProcQuoted class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The function to apply.</param>
        private EvaluateProcQuoted(Stepper caller, object expr, Environment env, Procedure fn)
            : base(caller, expr, env, fn)
        {
            ContinueStep(expr);
            ContinueHere(this.ApplyStep);
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
        /// Call apply proc evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="fn">The function to apply.</param>
        /// <returns>The apply proc evaluator.</returns>
        public static new Stepper Call(Stepper caller, object expr, Procedure fn)
        {
            return new EvaluateProcQuoted(caller, expr, caller.Env, fn);
        }
    }
}