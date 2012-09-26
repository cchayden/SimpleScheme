// <copyright file="EvaluateExpressionWithCatch.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate an expression with the ability to catch suspension.
    /// Anything that this calls that returns a suspended evaluator will be "caught"
    ///   by this evaluator.
    /// This will only catch the first suspension -- subsequent ones go through.
    /// Once caught, Undefined is returned.
    /// After suspension, the final evaluation result is returned along with a flag indicating its asynchronous nature.
    /// Evaluations can finish synchronously, without suspending, in which case it returns
    ///   in the normal way, with a return value.
    /// </summary>
    internal sealed class EvaluateExpressionWithCatch : Evaluator
    {
        #region Fields
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper initialStep = GetStepper("InitialStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper doneStep = GetStepper("DoneStep");

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-expression-with-catch");

        /// <summary>
        /// Indicates whether to catch suspensions.
        /// Catch only the first: let the rest through.
        /// </summary>
        private bool catchSuspended;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateExpressionWithCatch class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateExpressionWithCatch(SchemeObject expr, Environment env, Evaluator caller)
            : base(initialStep, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            this.catchSuspended = true;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Tells the evaluator main loop to catch suspensions.
        /// This is reset after we catch one, to let the others through.
        /// </summary>
        /// <returns>Whether we want to catch suspensions.</returns>
        internal override bool CatchSuspended
        {
            get { return this.catchSuspended; }
        }
        #endregion

        #region Call
        /// <summary>
        /// Creates an expression evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The expression evaluator.</returns>
        internal static EvaluateExpressionWithCatch Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return new EvaluateExpressionWithCatch(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the expression.
        /// </summary>
        /// <returns>Steps to evaluate the expression.</returns>
        protected override Evaluator InitialStep()
        {
            this.Pc = doneStep;
            return EvaluateExpression.Call(this.Expr, this.Env, this);
        }

        /// <summary>
        /// Back here after the expression has been evaluated.
        /// Return to caller.
        /// </summary>
        /// <returns>Execution continues with the return.</returns>
        protected override Evaluator DoneStep()
        {
            // If we get here because a suspend was caught, then return Undefined
            // and set a return flag so that the caller can recognize it.
            // Then do not catch any further suspensions.
            // If we get here because of a normal or asynchronous return, then set
            // the return value and set an appropriate flag value.
            Evaluator caller = this.Caller;
            Contract.Assert(caller != null);
            caller.ReturnedExpr = this.ReturnedExpr;
            ReturnType returnType;
            if (this.FetchAndResetCaught() > 0)
            {
                this.catchSuspended = false;
                returnType = ReturnType.CaughtSuspended;
            }
            else
            {
                returnType = this.catchSuspended ? ReturnType.SynchronousReturn : ReturnType.AsynchronousReturn;
            }

            caller.UpdateReturnFlag(returnType);
            return caller;
        }
        #endregion
    }
}