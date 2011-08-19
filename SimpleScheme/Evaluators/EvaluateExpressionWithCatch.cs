﻿#define OLD
// <copyright file="EvaluateExpressionWithCatch.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate an expression with the ability to catch suspension.
    /// Anything that this calls that returns a suspended evaluator will be "caught"
    ///   by this evaluator.
    /// This will only catch the first suspension -- subsequent ones go through.
    /// Once caught, a special undefined with value is returned.
    /// The final evaluation result is also returned as a different undefined value.
    /// Evaluations can finish synchronously, without suspending, in which case they return
    ///   the result of evaluating the given expression.
    /// </summary>
    public sealed class EvaluateExpressionWithCatch : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-expression-with-catch";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

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
        private EvaluateExpressionWithCatch(Obj expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            this.catchSuspended = true;
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Tells the evaluator main loop to catch suspensions.
        /// This is reset after we catch one, to let the others through.
        /// </summary>
        /// <returns>Whether we want to catch suspensions.</returns>
        public override bool CatchSuspended
        {
            get { return this.catchSuspended; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates an expression evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The expression evaluator.</returns>
        public static EvaluateExpressionWithCatch Call(Obj expr, Environment env, Evaluator caller)
        {
            return new EvaluateExpressionWithCatch(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate the expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Steps to evaluate the expression.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            return EvaluateExpression.Call(s.Expr, s.Env, s.ContinueHere(DoneStep));
        }

        /// <summary>
        /// Back here after the expression has been evaluated.
        /// Return to caller.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues with the return.</returns>
        private static Evaluator DoneStep(Evaluator s)
        {
            // If we get here because a suspend was caught, then return a distinguished
            //  value to the caller.  Make sure we do not catch any more suspensions
            //  from the same evaluation, and reset the counter so we can recognize the
            //  final return.
            // If we get here because of a normal return, then wrap return value and return 
            //  an appropriate catch code.
            var step = (EvaluateExpressionWithCatch)s;
            if (s.Caught > 0)
            {
                step.catchSuspended = false;
                s.ResetCaught();
                return s.ReturnCatchCode(AsyncReturnValue.CatchCode.CaughtSuspended, new Undefined());
            }

            return s.ReturnCatchCode(
                step.catchSuspended
                    ? AsyncReturnValue.CatchCode.NormalReturn
                    : AsyncReturnValue.CatchCode.ReturnAfterSuspended,
                s.ReturnedExpr);
        }

        #endregion
    }
}