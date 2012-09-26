// <copyright file="EvaluateAnd.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a sequence by evaluating each member.
    /// If a value is #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
    internal sealed class EvaluateAnd : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-and");
        #endregion

        #region Call
        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The and evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);

            // If no expr, avoid creating an evaluator.
            if (expr is EmptyList)
            {
                caller.ReturnedExpr = SchemeBoolean.True;
                return caller;
            }

            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalTestStep()
        {
            Contract.Assert(this.Expr != null);
            if (Rest(this.Expr) is EmptyList)
            {
                // On the last test, return directly to the caller, but use
                //  the current env.  This is to achieve tail recursion.
                var f = First(this.Expr);
                var ev = this.Env;
                var c = this.Caller;
                this.Reclaim();
                return EvaluateExpression.Call(f, ev, c);
            }

            this.Pc = OpCode.Loop;
            return EvaluateExpression.Call(First(this.Expr), this.Env, this);
        }

        /// <summary>
        /// If the evaluated expression is false, we are done.
        /// Otherwise, move down the list and try again at EvalTestStep.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator LoopStep()
        {
            if (SchemeBoolean.IsFalse(this.ReturnedExpr))
            {
                return this.ReturnFromEvaluator(SchemeBoolean.False);
            }

            this.Expr = Rest(this.Expr);

            // now do EvalTestStep
            return this.EvalTestStep();
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateAnd class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateAnd New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateAnd>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateAnd class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Newly initialized evaluator.</returns>
        private EvaluateAnd Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            Initialize(OpCode.EvalTest, expr, env, caller, counter);
            return this;
        }
        #endregion
    }
}