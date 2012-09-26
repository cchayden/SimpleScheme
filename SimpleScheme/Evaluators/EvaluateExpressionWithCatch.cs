// <copyright file="EvaluateExpressionWithCatch.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
            : base(InitialStep, expr, env, caller, counter)
        {
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
            return new EvaluateExpressionWithCatch(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Steps to evaluate the expression.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            s.Pc = DoneStep;
            return EvaluateExpression.Call(s.Expr, s.Env, s);
        }

        /// <summary>
        /// Back here after the expression has been evaluated.
        /// Return to caller.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with the return.</returns>
        private static Evaluator DoneStep(Evaluator s)
        {
            // If we get here because a suspend was caught, then return Undefined
            // and set a return flag so that the caller can recognize it.
            // Then do not catch any further suspensions.
            // If we get here because of a normal or asynchronous return, then set
            // the return value and set an appropriate flag value.
            var step = (EvaluateExpressionWithCatch)s;
            Evaluator caller = step.Caller;
            caller.ReturnedExpr = s.ReturnedExpr;
            ReturnType returnType;
            if (step.FetchAndResetCaught() > 0)
            {
                step.catchSuspended = false;
                returnType = ReturnType.CaughtSuspended;
            }
            else
            {
                returnType = step.catchSuspended ? ReturnType.SynchronousReturn : ReturnType.AsynchronousReturn;
            }

            caller.UpdateReturnFlag(returnType);
            return caller;
        }
        #endregion
    }
}