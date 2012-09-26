// <copyright file="EvaluateIf.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate an if expression.
    /// Evaluate the first part, then depending on its truth value, either
    ///   evaluate the second or third part.
    /// </summary>
    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
    internal sealed class EvaluateIf : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-if");
        #endregion

        #region Call
        /// <summary>
        /// Creates an if evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The if evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalTestStep()
        {
            this.Pc = OpCode.EvalAlternative;
            return EvaluateExpression.Call(First(this.Expr), this.Env, this);
        }

        /// <summary>
        /// Back here after the test has been evaluated.
        /// Evaluate and return either the second or third expression.
        /// If there is no third (or second), the value is undefined.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalAlternativeStep()
        {
            SchemeObject toEvaluate = SchemeBoolean.Truth(this.ReturnedExpr).Value ? Second(this.Expr) : Third(this.Expr);
            if (toEvaluate is EmptyList)
            {
                return this.ReturnFromEvaluator(Undefined.Instance);
            }

            Environment ev = this.Env;
            Evaluator c = this.Caller;
            this.Reclaim();
            return EvaluateExpression.Call(toEvaluate, ev, c);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Initializes a new instance of the EvaluateIf class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateIf Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            base.Initialize(OpCode.EvalTest, expr, env, caller, counter);
            return this;
        }

        /// <summary>
        /// Creates and initializes a new instance of the EvaluateIf class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateIf New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateIf>().Initialize(expr, env, caller);
        }
        #endregion
    }
}
