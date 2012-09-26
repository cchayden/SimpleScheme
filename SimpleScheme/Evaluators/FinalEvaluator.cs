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
        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the FinalEvaluator class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>Initialized evaluator.</returns>
        internal static FinalEvaluator New(SchemeObject expr)
        {
            Contract.Requires(expr != null);
            return GetInstance<FinalEvaluator>().Initialize(expr);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="FinalEvaluator"/> class. 
        /// </summary>
        /// <param name="expr">The value to return as the final result.</param>
        private FinalEvaluator Initialize(SchemeObject expr) 
        {
            Contract.Requires(expr != null);
            this.ReturnedExpr = expr;
            this.Pc = OpCode.End;
            return this;
        }
        #endregion

        #region Steps
        /// <summary>
        /// The step that ends evaluation
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EndStep()
        {
            var res = this.ReturnedExpr;
            this.Interp.SetComplete(res);
            this.ReturnedExpr = res;
            return this;
        }
        #endregion
    }
}