// <copyright file="EvaluateDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a define expression.
    //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
    //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
    //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>
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
        /// Handle the two forms of define.
        /// In the first case, just save the closure and return.
        /// This is what would result if we prepend "lambda" and call EvaluateExpression.
        /// In the second case, we need create an evaluator to evaluate the expression.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The define evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            if (Pair.Is(List.First(expr)))
            {
                // Defun case -- create a closure and bind it to the variable.
                env.UnsafeDefine(List.First(List.First(expr)), new Closure(List.Rest(List.First(expr)), List.Rest(expr), env));
                return caller.ContinueStep(Undefined.Instance);
            }

            return new EvaluateDefine(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start by evaluating the expression.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Continue by evaluating the expression.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            return EvaluateExpression.Call(List.Second(s.Expr), s.Env, s.ContinueHere(StoreDefineStep));
        }

        /// <summary>
        /// Back from expression evaluation.  Store the result as the value of the symbol
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Execution continues in the caller.</returns>
        private static Stepper StoreDefineStep(Stepper s)
        {
            s.Env.UnsafeDefine(List.First(s.Expr), s.ReturnedExpr);
            return s.ReturnUndefined();
        }
        #endregion
    }
}