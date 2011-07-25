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
        /// The step and its chain of steps back to the beginning have to be cloned because they
        ///   hold information about the progress of the evaluation.  When the evaluation proceeds
        ///   these steps might be altered, damaging the ability to continue, which is what makes the
        ///   clone necessary.
        /// </summary>
        /// <param name="step">The continuation to return to when applied.</param>
        internal Continuation(Stepper step)
        {
            this.step = step.Caller.CloneChain(); 
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
            return string.Empty;
        }
        #endregion

        #region Internal Methods

        /// <summary>
        /// Execute the continuation.
        /// Transfers execution to the step saved when the continuation was created.
        /// The environment in effect at that time is also restored.
        /// Again, the chain of steps back to the beginning need to be clones so that this application
        ///   does not alter the evaluation, making it impossible to return back to the continuation.
        /// </summary>
        /// <param name="args">The value to return.</param>
        /// <param name="caller">The calling evaluator.  Not used, since control is transferred away.</param>
        /// <returns>The next step to execute.</returns>
        internal override Stepper Apply(object args, Stepper caller)
        {
            return Stepper.TransferToStep(this.step.CloneChain(), List.First(args), this.step.Env);
        }
        #endregion
    }
}
