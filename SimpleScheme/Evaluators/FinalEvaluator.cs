// <copyright file="FinalEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// This is at the base of the chain of evaluators.
    /// </summary>
    internal class FinalEvaluator : Evaluator
    {
        #region Fields
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper endStep = GetStepper("EndStep");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the <see cref="FinalEvaluator"/> class. 
        /// </summary>
        /// <param name="expr">The value to return as the final result.</param>
        internal FinalEvaluator(SchemeObject expr) 
        {
            Contract.Requires(expr != null);
            this.ReturnedExpr = expr;
            this.Pc = endStep;
        }
        #endregion

        #region Steps
        /// <summary>
        /// The step that ends evaluation
        /// </summary>
        /// <returns>The next evaluator to execute.</returns>
        protected override Evaluator EndStep()
        {
            this.Interp.SetComplete(this.ReturnedExpr);
            return new FinalEvaluator(this.ReturnedExpr);
        }
        #endregion
    }
}