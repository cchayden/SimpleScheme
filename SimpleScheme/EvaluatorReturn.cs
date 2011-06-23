// <copyright file="EvaluatorReturn.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// This evaluator is used to signal the end of evaulation.
    /// This is the evaluator that the top level evaulator "returns" to.
    /// </summary>
    public class EvaluatorReturn : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorReturn class.
        /// </summary>
        public EvaluatorReturn()
            : base(null, null, null, null)
        {
        }

        /// <summary>
        /// This evaluator should never execute steps.
        /// </summary>
        /// <returns>Throws an exception -- does not return.</returns>
        public override Evaluator EvalStep()
        {
            throw new Exception("Do not call RvaluatorReturn::EvalStep");
        }
    }
}