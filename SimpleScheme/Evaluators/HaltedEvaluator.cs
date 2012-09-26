// <copyright file="HaltedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// This evaluator is returned to halt evaluation.
    /// It is used as the base evaluator, returned to after everything is done.
    /// </summary>
    public class HaltedEvaluator : Evaluator
    {
        /// <summary>
        /// The printable name of the evaluator type.
        /// </summary>
        public const string EvaluatorName = "";

        /// <summary>
        /// Initializes a new instance of the HaltedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluator environment.</param>
        public HaltedEvaluator(Environment env) : 
            base(null, env, null)
        {
        }

        /// <summary>
        /// Break out of the stepper loop.
        /// Set the async completion code, if appropriate.
        /// </summary>
        /// <returns>Null causes the main loop to break.</returns>
        public override Evaluator Divert()
        {
            this.Interp.SetComplete(this.ReturnedExpr);
            return null;
        }

        /// <summary>
        /// Write the evaluator to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public new void PrintString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<halted-evaluator>");
            }
        }
    }
}
