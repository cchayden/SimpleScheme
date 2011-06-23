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
        /// The macro to expand.
        /// </summary>
        private readonly Macro fn;

        /// <summary>
        /// Initializes a new instance of the EvaluateExpandMacro class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The macro to expand.</param>
        private EvaluateExpandMacro(Stepper parent, object expr, Environment env, Macro fn)
            : base(parent, expr, env)
        {
            this.fn = fn;
            this.Pc = this.InitialStep;
            IncrementCounter("expand-macro");
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
            this.Pc = this.ExpandStep;
            return this.fn.Apply(this, Expr);
        }

        /// <summary>
        /// Back here after macro is expanded.  Evaluate the result.
        /// </summary>
        /// <returns>The expanded macro.</returns>
        private Stepper ExpandStep()
        {
            this.Pc = this.ReturnStep;
            return EvaluatorMain.Call(this, ReturnedExpr);
        }
    }
}