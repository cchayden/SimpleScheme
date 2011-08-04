﻿// <copyright file="EvaluateExpandMacro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Expand a macro.
    /// </summary>
    public sealed class EvaluateExpandMacro : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-expand-macro";

        /// <summary>
        /// The macro to expand.
        /// </summary>
        private readonly Macro fn;

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateExpandMacro class.
        /// </summary>
        /// <param name="fn">The macro to expand.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateExpandMacro(Macro fn, Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.fn = fn;
            ContinueHere(ExpandStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call an expand evaluator.
        /// This comes here with a macro (fn) and a set of macro arguments (args).
        /// The args are unevaluated.  
        /// The macro is applied to the unevaluated arguments, yielding an expanded program.
        /// Then the result is evaluated as an expression.
        /// </summary>
        /// <param name="fn">The macro to expand.</param>
        /// <param name="args">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The expand evaluator.</returns>
        public static Stepper Call(Macro fn, Obj args, Environment env, Stepper caller)
        {
            return new EvaluateExpandMacro(fn, args, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Apply the macro to the arguments, expanding it.  
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The first step to expand to macro.</returns>
        private static Stepper ExpandStep(Stepper s)
        {
            EvaluateExpandMacro step = (EvaluateExpandMacro)s;
            return step.fn.Apply(s.Expr, s.ContinueHere(EvaluateStep));
        }

        /// <summary>
        /// Back here after macro is expanded.  Evaluate the result.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Return to caller with the expanded macro.</returns>
        private static Stepper EvaluateStep(Stepper s)
        {
            return EvaluateExpression.Call(s.ReturnedExpr, s.Env, s.Caller);
        }
        #endregion
    }
}