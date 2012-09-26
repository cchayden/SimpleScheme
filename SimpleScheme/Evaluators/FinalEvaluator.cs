// <copyright file="FinalEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// This is used to end execution.  The evaluation result is in ReturnedExpr.
    /// </summary>
    internal class FinalEvaluator : Evaluator
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the FinalEvaluator class.
        /// </summary>
        /// <param name="expr">The expression to return.</param>
        internal FinalEvaluator(SchemeObject expr)
            : base(null, null, null, null)
        {
            this.ReturnedExpr = expr;
            this.Finished = true;
        }
        #endregion
    }
}
