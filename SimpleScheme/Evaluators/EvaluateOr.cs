﻿// <copyright file="EvaluateOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
    public sealed class EvaluateOr : Evaluator
    {
        #region Fields
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
            : base(expr, env, caller)
        {
            ContinueHere(EvalTestStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Calls an or evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The or evaluator.</returns>
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            // If no expr, avoid creating an evaluator.
            if (expr is EmptyList)
            {
                return caller.UpdateReturnValue((SchemeBoolean)false);
            }

            return new EvaluateOr(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Steps to evaluate the expression.</returns>
        private static Evaluator EvalTestStep(Evaluator s)
        {
            if (Rest(s.Expr) is EmptyList)
            {
                // On the last test, return directly to the caller, but use
                //  the current env.  This is to achieve tail recursion.
                return EvaluateExpression.Call(First(s.Expr), s.Env, s.Caller);
            }

            return EvaluateExpression.Call(First(s.Expr), s.Env, s.ContinueHere(LoopStep));
        }

        /// <summary>
        /// If the expression evaluated to true, we are done and we can return the expression value.
        /// Otherwise, move down the list.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The evaluation result, or loops back to evaluate the next item.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            if (SchemeBoolean.Truth(s.ReturnedExpr).Value)
            {
                return s.ReturnFromStep(s.ReturnedExpr);
            }

            s.StepDownExpr();
            return s.ContinueHere(EvalTestStep);
        }
        #endregion
    }
}