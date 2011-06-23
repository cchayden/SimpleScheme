// <copyright file="EvaluatorBase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The initial evaluator.  Eval creates one of these to be the parent of its
    ///     initial step.
    /// </summary>
    public class EvaluatorBase : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorBase class.
        /// This is never applied.
        /// </summary>
        /// <param name="name">The evaluator name.</param>
        public EvaluatorBase(string name)
            : base(null, name, null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the EvaluatorBase class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        protected EvaluatorBase(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// This should never be called.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            return ErrorHandlers.EvalError("Base: cannot run.");
        }
    }
}
