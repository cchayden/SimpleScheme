﻿// <copyright file="EvaluateAnd.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence by evaluating each member.
    /// If a value is #f then return it.  Otherwise return the last value.
    /// </summary>
    //// <r4rs section="4.2.1">(and <test1> ...)</r4rs>
    public sealed class EvaluateAnd : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-and");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateAnd class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateAnd(SchemeObject expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            this.ContinueHere(EvalTestStep);
            this.IncrementCounter(counter);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The and evaluator.</returns>
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            // If no expr, avoid creating an evaluator.
            if (expr is EmptyList)
            {
                return caller.UpdateReturnValue((SchemeBoolean)true);
            }

            return new EvaluateAnd(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate the next test expression in the list.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator to execute.</returns>
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
        /// If the evaluated expression is false, we are done.
        /// Otherwise, move down the list and try again at EvalTestStep.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The result, or this evaluator to loop back to previous step.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            if (SchemeBoolean.IsFalse(EnsureSchemeObject(s.ReturnedExpr)))
            {
                return s.ReturnFromStep((SchemeBoolean)false);
            }

            s.StepDownExpr();
            return s.ContinueHere(EvalTestStep);
        }
        #endregion
    }
}