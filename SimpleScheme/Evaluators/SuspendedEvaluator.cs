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
    public class SuspendedEvaluator : Evaluator
    {
        /// <summary>
        /// The printable name of the evaluator type.
        /// </summary>
        public const string EvaluatorName = "suspended-evaluator";

        /// <summary>
        /// Initializes a new instance of the SuspendedEvaluator class.
        /// It is used to indicate that an evaluation has suspended rather than returning a value.
        /// The caller is needed so that we can search for a catcher.
        /// The Evaluator is given as the ReturnedExpr of the Evaluator.
        /// </summary>
        /// <param name="caller">The calling evaluator.</param>
        public SuspendedEvaluator(Evaluator caller) : 
            base(null, null, caller)
        {
            this.UpdateReturnValue(this);
        }

        /// <summary>
        /// Divert execution if there is a suspension handler.
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
            return step;
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
