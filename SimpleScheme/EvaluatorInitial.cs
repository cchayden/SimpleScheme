// <copyright file="EvaluatorInitial.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// The initial evaluator.  Eval creates one of these to be the parent of its
        ///     initial step.
        /// </summary>
        private class EvaluatorInitial : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorInitial class.
            /// This is never applied.
            /// </summary>
            public EvaluatorInitial()
                : base(null, null, null)
            {
            }

            /// <summary>
            /// This should never be called.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper RunStep()
            {
                return EvalError("Initial: program counter error");
            }
        }
    }
}
