// <copyright file="EvaluateExpandMacro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Expand a macro.
    /// </summary>
    public sealed class EvaluateExpandMacro : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-expand-macro";

        /// <summary>
        /// The macro to expand.
        /// </summary>
        private readonly Macro fn;

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);
        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the EvaluateExpandMacro class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="fn">The macro to expand.</param>
        private EvaluateExpandMacro(object expr, Environment env, Evaluator caller, Macro fn)
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
        public static Evaluator Call(Macro fn, Obj args, Environment env, Evaluator caller)
        {
            return new EvaluateExpandMacro(args, env, caller, fn);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Apply the macro to the arguments, expanding it.  
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The step to evaluate the expanded macro.</returns>
        private static Evaluator ExpandStep(Evaluator s)
        {
            EvaluateExpandMacro step = (EvaluateExpandMacro)s;
            return step.fn.Apply(s.Expr, s.ContinueHere(EvaluateStep));
        }

        /// <summary>
        /// Back here after macro is expanded.  Evaluate the result.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Return to caller with the expanded macro.</returns>
        private static Evaluator EvaluateStep(Evaluator s)
        {
            return EvaluateExpression.Call(s.ReturnedExpr, s.Env, s.Caller);
        }
        #endregion
    }
}