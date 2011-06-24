// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Represents a continuation.
    /// </summary>
    internal sealed class Continuation : Procedure
    {
        #region Fields
        /// <summary>
        /// The step to execute when the continuation is applied.
        /// </summary>
        private readonly Stepper step;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Continuation class.
        /// </summary>
        /// <param name="step">The continuation to return to when applied.</param>
        private Continuation(Stepper step)
        {
            this.step = step.Caller;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the continuation as a string.  
        /// Displays the body, as it has been processed by the reader.
        /// </summary>
        /// <returns>The string form of the continuation.</returns>
        public override string ToString()
        {
            return string.Format("call-with-current-continuation {0}", this.step.Expr);
        }
        #endregion

        #region Static Internal Methods
        /// <summary>
        /// Create a new continuation.
        /// </summary>
        /// <param name="step">The continuation to return to when applied.</param>
        /// <returns>The continuation.</returns>
        internal static Continuation New(Stepper step)
        {
            return new Continuation(step);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Execute the continuation.
        /// Transfers execution to the step saved when the continuation was created.
        /// The environment in effect at that time is also restored.
        /// </summary>
        /// <param name="args">The value to return.</param>
        /// <param name="env">The environment to use for the application.</param>
        /// <param name="caller">The calling evaluator.  Not used, since control is transferred away.</param>
        /// <returns>The next step to execute.</returns>
        internal override Stepper Apply(Obj args, Environment env, Stepper caller)
        {
            return Stepper.TransferToStep(this.step, First(args), this.step.Env);
        }
        #endregion
    }
}
