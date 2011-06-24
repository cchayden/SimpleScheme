// <copyright file="EvaluateSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a set! expression.
    /// </summary>
    //// <r4rs section="4.1.6">(set <variable> <expression>)</r4rs>
    internal sealed class EvaluateSet : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-set";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the EvaluateSet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateSet(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Calls a set evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The set evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateSet(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate the second expression (rhs).
        /// </summary>
        /// <returns>Code to evaluate the second expression.</returns>
        private Stepper InitialStep()
        {
            return EvaluateExpression.Call(Second(Expr), this.Env, ContinueHere(this.SetStep));
        }

        /// <summary>
        /// Back here after evaluation.  Assign the result to the variable in the environment
        ///   named by the first part of the expression.
        /// </summary>
        /// <returns>Returns to caller.</returns>
        private Stepper SetStep()
        {
            this.Env.Set(First(Expr), ReturnedExpr);
            return ReturnUndefined();
        }
        #endregion
    }
}