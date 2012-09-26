// <copyright file="EvaluateOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
    internal sealed class EvaluateOr : Evaluator
    {
        #region Fields
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper evalTestStep = GetStepper("EvalTestStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper loopStep = GetStepper("LoopStep");

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-or");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateOr class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateOr(SchemeObject expr, Environment env, Evaluator caller)
            : base(evalTestStep, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
        }
        #endregion

        #region Call
        /// <summary>
        /// Calls an or evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The or evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);

            // If no expr, avoid creating an evaluator.
            if (expr is EmptyList)
            {
                caller.ReturnedExpr = (SchemeBoolean)false;
                return caller;
            }

            return new EvaluateOr(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <returns>Steps to evaluate the expression.</returns>
        protected override Evaluator EvalTestStep()
        {
            if (Rest(this.Expr) is EmptyList)
            {
                // On the last test, return directly to the caller, but use
                //  the current env.  This is to achieve tail recursion.
                return EvaluateExpression.Call(First(this.Expr), this.Env, this.Caller);
            }

            this.Pc = loopStep;
            return EvaluateExpression.Call(First(this.Expr), this.Env, this);
        }

        /// <summary>
        /// If the expression evaluated to true, we are done and we can return the expression value.
        /// Otherwise, move down the list.
        /// </summary>
        /// <returns>The evaluation result, or loops back to evaluate the next item.</returns>
        protected override Evaluator LoopStep()
        {
            if (SchemeBoolean.Truth(this.ReturnedExpr).Value)
            {
                Evaluator caller = this.Caller;
                Contract.Assert(caller != null);
                caller.ReturnedExpr = this.ReturnedExpr;
                return caller;
            }

            this.Expr = Rest(this.Expr);
            this.Pc = evalTestStep;
            return this;
        }
        #endregion
    }
}