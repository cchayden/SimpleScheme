// <copyright file="EndedEvaluator.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// This evaluator is returned to halt evaluation.
    /// It is used after an asynchronous fork.
    /// </summary>
    public class EndedEvaluator : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EndedEvaluator class.
        /// </summary>
        /// <param name="env">The evaluator environment.</param>
        public EndedEvaluator(Environment env) : 
            base(null, env, null)
        {
        }

        /// <summary>
        /// Always breaks out of the interpreter loop.
        /// </summary>
        /// <returns>Null return causes stepper to return.</returns>
        public override Evaluator Divert()
        {
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
                buf.Append("<ended-evaluator>");
            }
        }
    }
}
