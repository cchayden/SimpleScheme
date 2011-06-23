// <copyright file="EvaluatorBase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The initial evaluator.  
    /// Eval creates one of these to be the parent of its initial step.
    /// It is also used for a suspended evaluator in async operations.
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
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        protected EvaluatorBase(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return "base"; }
        }
    }
}
