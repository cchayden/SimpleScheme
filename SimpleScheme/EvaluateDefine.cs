// <copyright file="EvaluateDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    internal sealed class EvaluateDefine : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-define";

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
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Call a define evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The define evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
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
        /// <returns>Continue by evaluating the body of the definition.</returns>
        private Stepper InitialStep()
        {
            if (TypePrimitives.IsPair(First(Expr)))
            {
                this.Env.Define(First(First(Expr)), new Closure(Rest(First(Expr)), Rest(Expr), this.Env));
                return ReturnUndefined();
            }

            return EvaluateExpression.Call(Second(Expr), this.Env, ContinueHere(this.StoreDefine));
        }

        /// <summary>
        /// Back from defun.  Store the body as the value of the symbol
        /// </summary>
        /// <returns>Execution continues in the caller.</returns>
        private Stepper StoreDefine()
        {
            this.Env.Define(First(Expr), ReturnedExpr);
            return ReturnUndefined();
        }
        #endregion
    }
}