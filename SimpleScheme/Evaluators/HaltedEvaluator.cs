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
        #region Initialize
        /// <summary>
        /// Initializes a new instance of the HaltedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluator environment.</param>
        internal HaltedEvaluator Initialize(Environment env)
        {
            Contract.Requires(env != null);
            Initialize(OpCode.End, Undefined.Instance, env, FinalEvaluator.New(Undefined.Instance));
            return this;
        }

        /// <summary>
        /// Creates and initializes a new instance of the HaltedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluation environment</param>
        /// <returns>Initialized evaluator.</returns>
        internal static HaltedEvaluator New(Environment env)
        {
            Contract.Requires(env != null);
            return new HaltedEvaluator().Initialize(env);
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
            return this.ReturnFromEvaluator(res);
        }
        #endregion
    }
}
