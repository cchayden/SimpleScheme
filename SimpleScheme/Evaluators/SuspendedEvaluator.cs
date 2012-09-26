// <copyright file="SuspendedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// This evaluator is returned to suspend evaluation.
    /// It is used after calling an asynchronous operation.
    /// </summary>
    internal sealed class SuspendedEvaluator : Evaluator
    {
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
        /// <param name="caller">The calling evaluator.</param>
        internal SuspendedEvaluator(SchemeObject res, Evaluator caller) : 
            base(null, res, null, caller)
        {
        }

        /// <summary>
        /// Divert execution if there is a suspension handler.
        /// Pass the async result to the resumed step.
        /// </summary>
        /// <returns>Null to return from main loop, or else the step to run next.</returns>
        internal Evaluator NextStep()
        {
            // See if evaluator wants to handle
            Evaluator step = SearchForHandler(this);
            if (step == null)
            {
                // nope -- just finish up
                return new FinalEvaluator(ClrObject.New(this));
            }

            // this evaluator wants to handle -- run it now
            step.IncrementCaught();
            step.ReturnedExpr = this.Expr;  // the AsyncResult
            return step;
        }

        /// <summary>
        /// When a step is suspended, check with each caller up the chain, seeing if any
        ///   one of them want to resume.
        /// </summary>
        /// <param name="step">The evaluator where the search starts.</param>
        /// <returns>The evaluator that wants to handle suspension, orherwise null</returns>
        private static Evaluator SearchForHandler(Evaluator step)
        {
            while (step != null)
            {
                if (step.CatchSuspended)
                {
                    return step;
                }

                step = step.Caller;
            }

            return null;
        }
    }
}
