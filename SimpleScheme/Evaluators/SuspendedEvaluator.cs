// <copyright file="SuspendedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// This evaluator is returned to suspend evaluation.
    /// It is used after calling an asynchronous operation.
    /// </summary>
    internal sealed class SuspendedEvaluator : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("suspended");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the SuspendedEvaluator class.
        /// It is used to indicate that an evaluation has suspended rather than returning a value.
        /// The caller is needed so that we can search for a catcher.
        /// The SuspendedEvaluator is given as the ReturnedExpr of the evaluaton, and of the whole Eval.
        /// The AsyncResult can be extracted from this if necessary, but is NOT the final async result,
        ///   just the intermediate result of this suspension.
        /// AsyncResult is stored in Expr.
        /// </summary>
        /// <param name="res">The IAsyncResult associated with the suspension.</param>
        /// <param name="env">The environment to use when returning from suspension.</param>
        /// <param name="caller">The calling evaluator.</param>
        internal SuspendedEvaluator(SchemeObject res, Environment env, Evaluator caller) : 
            base(OpCode.ContinueAfterSuspended, res, env, caller, counter)
        {
            Contract.Requires(res != null);
            Contract.Requires(caller != null);
            this.ReturnedExpr = ClrObject.New(this);
        }
        #endregion

        #region Steps
        /// <summary>
        /// The step that ends evaluation
        /// </summary>
        /// <returns>The next evaluator to execute.</returns>
        protected override Evaluator ContinueAfterSuspendedStep()
        {
            // See if evaluator wants to handle
            Evaluator step = this.SearchForHandler();
            if (step == null)
            {
                // nope -- just break
                return new FinalEvaluator(this.ReturnedExpr);
            }

            // this evaluator wants to handle -- run it now
            step.IncrementCaught();
            step.ReturnedExpr = this.Expr;  // the AsyncResult
            return step;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// When a step is suspended, check with each caller up the chain, seeing if any
        ///   one of them want to resume.
        /// </summary>
        /// <returns>The evaluator that wants to handle suspension, orherwise null</returns>
        private Evaluator SearchForHandler()
        {
            Evaluator step = this;
            while (!(step is FinalEvaluator))
            {
                if (step.CatchSuspended)
                {
                    return step;
                }

                step = step.Caller;
            }

            return null;
        }
        #endregion
    }
}
