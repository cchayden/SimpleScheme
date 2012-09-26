// <copyright file="EvaluateIf.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate an if expression.
    /// Evaluate the first part, then depending on its truth value, either
    ///   evaluate the second or third part.
    /// </summary>
    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
    public sealed class EvaluateIf : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-if");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateIf class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateIf(SchemeObject expr, Environment env, Evaluator caller)
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
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            return new EvaluateIf(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Steps to evaluate the test.</returns>
        private static Evaluator EvalTestStep(Evaluator s)
        {
            return EvaluateExpression.Call(First(s.Expr), s.Env, s.ContinueHere(EvalAlternativeStep));
        }

        /// <summary>
        /// Back here after the test has been evaluated.
        /// Evaluate and return either the second or third expression.
        /// If there is no thid, the empty list will be evaluated, which is OK.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with the return.</returns>
        private static Evaluator EvalAlternativeStep(Evaluator s)
        {
            EvaluatorOrObject toEvaluate = SchemeBoolean.Truth(EnsureSchemeObject(s.ReturnedExpr)).Value ? Second(s.Expr) : Third(s.Expr);
            return EvaluateExpression.Call(
                toEvaluate is EmptyList ? Undefined.Instance : toEvaluate, s.Env, s.Caller);
        }
        #endregion
    }
}
