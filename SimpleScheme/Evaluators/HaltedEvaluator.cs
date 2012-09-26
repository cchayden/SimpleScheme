// <copyright file="HaltedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// This evaluator is returned to halt evaluation.
    /// It is used as the base evaluator, returned to after everything is done.
    /// </summary>
    internal class HaltedEvaluator : Evaluator
    {
        #region Fields
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper endStep = GetStepper("EndStep");

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("halted");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the HaltedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluator environment.</param>
        internal HaltedEvaluator(Environment env) : 
            base(endStep, Undefined.Instance, env, new FinalEvaluator(Undefined.Instance), counter)
        {
            Contract.Requires(env != null);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        internal override string ToString(bool quoted)
        {
            return "<halted-evaluator>";
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
