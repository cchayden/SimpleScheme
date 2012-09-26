#define OLD
// <copyright file="HaltedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// This evaluator is returned to halt evaluation.
    /// It is used as the base evaluator, returned to after everything is done.
    /// </summary>
    internal class HaltedEvaluator : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the HaltedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluator environment.</param>
        internal HaltedEvaluator(Environment env) : 
            base(HaltStep, null, env, null)
        {
        }

        /// <summary>
        /// Halt evaluation by returning a HaltedEvaluator.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The FinalEvaluator, which stops evaluation.</returns>
        private static Evaluator HaltStep(Evaluator s)
        {
            s.Interp.SetComplete(s.ReturnedExpr);
            return new FinalEvaluator(s.ReturnedExpr);
        }
    }
}
