﻿// <copyright file="Continuation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Represents a continuation.
    /// </summary>
    public sealed class Continuation : Procedure
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
        public Continuation(Stepper step)
        {
            this.step = step.CallerCaller;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the value to return as the result of executing the continuation.
        /// </summary>
        public Obj Value { get; private set; }
        #endregion

        #region Public Methods
        /// <summary>
        /// Execute the continuation.
        /// Transfers execution to the step saved when the continuation was created.
        /// The environment in effect at that time is also restored.
        /// </summary>
        /// <param name="args">The value to return.</param>
        /// <param name="caller">The calling evaluator.  Not used, since control is transferred away.</param>
        /// <returns>The next step to execute.</returns>
        public override Stepper Apply(Obj args, Stepper caller)
        {
            this.Value = First(args);
            return Stepper.TransferToStep(this.step, First(args), this.step.Env);
        }

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
    }
}
