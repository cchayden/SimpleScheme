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
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("suspended");

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
            base(null, res, null, caller, counter)
        {
            this.ReturnedExpr = ClrObject.New(this);
        }

        /// <summary>
        /// Divert execution if there is a suspension handler.
        /// Pass the async result to the resumed step.
        /// </summary>
        /// <returns>Null to return from main loop, or else the step to run next.</returns>
        internal override Evaluator NextStep()
        {
            // See if evaluator wants to handle
            Evaluator step = this.SearchForHandler();
            if (step == null)
            {
                // nope -- just break
                return null;
            }

            // this evaluator wants to handle -- run it now
            step.IncrementCaught();
            step.ReturnedExpr = this.Expr;  // the AsyncResult
            return step;
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        internal override string ToString(bool quoted)
        {
            return "<suspended-evaluator>";
        }

        /// <summary>
        /// When a step is suspended, check with each caller up the chain, seeing if any
        ///   one of them want to resume.
        /// </summary>
        /// <returns>The evaluator that wants to handle suspension, orherwise null</returns>
        private Evaluator SearchForHandler()
        {
            Evaluator step = this;
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
