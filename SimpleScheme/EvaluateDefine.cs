// <copyright file="EvaluateDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    public sealed class EvaluateDefine : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-define";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateDefine class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateDefine(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call a define evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The define evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateDefine(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Handle the two forms of define.
        /// In the first case, just save the closure and return.
        /// This is what would result if we prepend "lambda" and call EvaluateExpression.
        /// In the second case (defun) start by evaluating the body.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Continue by evaluating the body of the definition.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            if (Pair.IsPair(List.First(s.Expr)))
            {
                s.Env.Define(List.First(List.First(s.Expr)), new Closure(List.Rest(List.First(s.Expr)), List.Rest(s.Expr), s.Env));
                return s.ReturnUndefined();
            }

            return EvaluateExpression.Call(List.Second(s.Expr), s.Env, s.ContinueHere(StoreDefineStep));
        }

        /// <summary>
        /// Back from defun.  Store the body as the value of the symbol
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Execution continues in the caller.</returns>
        private static Stepper StoreDefineStep(Stepper s)
        {
            s.Env.Define(List.First(s.Expr), s.ReturnedExpr);
            return s.ReturnUndefined();
        }
        #endregion
    }
}