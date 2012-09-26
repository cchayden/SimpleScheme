// <copyright file="EvaluateOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(or <test1> ...)</r4rs>
    internal sealed class EvaluateOr : Evaluator
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateOr class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateOr(SchemeObject expr, Environment env, Evaluator caller)
            : base(EvalTestStep, expr, env, caller)
        {
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

            s.Pc = LoopStep;
            return EvaluateExpression.Call(First(s.Expr), s.Env, s);
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
                Evaluator caller = s.Caller;
                caller.ReturnedExpr = s.ReturnedExpr;
                return caller;
            }

            s.Expr = Rest(s.Expr);
            s.Pc = EvalTestStep;
            return s;
        }
        #endregion
    }
}