// <copyright file="SuspendedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// This evaluator is returned to suspend evaluation.
    /// It is used after calling an asynchronous operation.
    /// </summary>
    public sealed class SuspendedEvaluator : Evaluator
    {
        /// <summary>
        /// The printable name of the evaluator type.
        /// </summary>
        public const string EvaluatorName = "suspended-evaluator";

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
        public SuspendedEvaluator(Obj res, Evaluator caller) : 
            base(res, null, caller)
        {
            this.UpdateReturnValue(this);
        }

        /// <summary>
        /// Divert execution if there is a suspension handler.
        /// Pass the async result to the resumed step.
        /// </summary>
        /// <returns>Null to return from main loop, or else the step to run next.</returns>
        public override Evaluator Divert()
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
            return step.UpdateReturnValue(this.Expr);  // the AsyncResult
        }

        /// <summary>
        /// Write the evaluator to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void AsString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<" + EvaluatorName + ">");
            }
        }
    }
}
