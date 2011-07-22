// <copyright file="EvaluatorBase.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// The initial evaluator.  
    /// Eval creates one of these to be the parent of its initial step.
    /// It is also used for a suspended evaluator in async operations.
    /// </summary>
    internal class EvaluatorBase : Stepper
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluatorBase class.
        /// This is never applied.
        /// </summary>
        /// <param name="name">The evaluator name.</param>
        internal EvaluatorBase(string name)
            : base(name, null, null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the EvaluatorBase class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        internal EvaluatorBase(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
        }
        #endregion
    }
}
