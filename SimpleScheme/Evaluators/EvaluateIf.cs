// <copyright file="EvaluateIf.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate an if expression.
    /// Evaluate the first part, then depending on its truth value, either
    ///   evaluate the second or third part.
    /// </summary>
    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
    public sealed class EvaluateIf : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-if";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateIf class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateIf(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(EvalTestStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates an if evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The if evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateIf(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Steps to evaluate the test.</returns>
        private static Stepper EvalTestStep(Stepper s)
        {
            return EvaluateExpression.Call(List.First(s.Expr), s.Env, s.ContinueHere(EvalAlternativeStep));
        }

        /// <summary>
        /// Back here after the test has been evaluated.
        /// Evaluate and return either the second or third expression.
        /// If there is no thid, the empty list will be evaluated, which is OK.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Execution continues with the return.</returns>
        private static Stepper EvalAlternativeStep(Stepper s)
        {
            Obj toEvaluate = SchemeBoolean.Truth(s.ReturnedExpr) ? List.Second(s.Expr) : List.Third(s.Expr);
            return EvaluateExpression.Call(
                EmptyList.Is(toEvaluate) ? Undefined.Instance : toEvaluate, s.Env, s.Caller);
        }
        #endregion
    }
}
