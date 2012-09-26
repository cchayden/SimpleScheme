// <copyright file="HaltedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("halted");

        /// <summary>
        /// Initializes a new instance of the HaltedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluator environment.</param>
        public HaltedEvaluator(Environment env) : 
            base(null, env, null, counter)
        {
        }

        /// <summary>
        /// Break out of the stepper loop.
        /// Set the async completion code, if appropriate.
        /// </summary>
        /// <returns>Null causes the main loop to break.</returns>
        public override Evaluator NextStep()
        {
            this.Interp.SetComplete(this.ReturnedExpr);
            return null;
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        public override string ToString(bool quoted)
        {
            return "<halted-evaluator>";
        }
    }
}
