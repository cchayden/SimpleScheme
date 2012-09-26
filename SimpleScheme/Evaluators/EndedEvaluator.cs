// <copyright file="EndedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// This evaluator is returned to halt evaluation.
    /// It is used after an asynchronous fork.
    /// </summary>
    public class EndedEvaluator : Evaluator
    {
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("ended");

        /// <summary>
        /// Initializes a new instance of the EndedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluator environment.</param>
        public EndedEvaluator(Environment env) : 
            base(null, env, null, counter)
        {
        }

        /// <summary>
        /// Always breaks out of the interpreter loop.
        /// </summary>
        /// <returns>Null return causes stepper to return.</returns>
        public override Evaluator NextStep()
        {
            return null;
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        public override string ToString(bool quoted)
        {
            return "<ended-evaluator>";
        }
    }
}
