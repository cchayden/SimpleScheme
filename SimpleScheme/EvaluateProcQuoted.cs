// <copyright file="EvaluateProcQuoted.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Apply a proc to args without evaluation.
    /// Used to evaluate cond expressions.
    /// </summary>
    internal sealed class EvaluateProcQuoted : EvaluateProc
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        internal new const string StepperName = "evaluate-proc-quoted";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateProcQuoted class.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateProcQuoted(Procedure fn, Obj expr, Environment env, Stepper caller)
            : base(fn, expr, env, caller)
        {
            ContinueStep(expr);
            ContinueHere(this.ApplyStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Call apply proc evaluator.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The apply proc evaluator.</returns>
        internal static new Stepper Call(Procedure fn, Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateProcQuoted(fn, expr, env, caller);
        }
        #endregion
    }
}