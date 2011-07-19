// <copyright file="EvaluateSequence.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a sequence by evaluating each member and returning the last value.
    /// </summary>
   //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
    internal sealed class EvaluateSequence : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-sequence";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The list of expressions.
        /// </summary>
        private Obj expressions;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateSequence class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateSequence(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.expressions = expr;
            ContinueHere(this.EvalExprStep);
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
        /// Call the sequence evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The sequence evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateSequence(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Initial step: to see if we are done.
        /// If not, evaluate the next expression.
        /// If we are, evaluate and return the last expr.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalExprStep()
        {
            if (TypePrimitives.IsEmptyList(Rest(this.expressions)))
            {
                // On the last expr in the sequence, return directly to the caller.
                // This is *crucial* for tail recursion.
                // If this instead continues to a "DoneStep" here that calls ReturnFromStep(ReturnedExpr) then each
                //   EvaluateSequence and each environment will be stacked up.  
                return EvaluateExpression.Call(First(this.expressions), this.Env, this.Caller);
            }

            return EvaluateExpression.Call(First(this.expressions), this.Env, ContinueHere(this.LoopStep));
        }

        /// <summary>
        /// Comes back here after expression evaluation.  Loop back and evaluate another
        /// </summary>
        /// <returns>Immediately steps back.</returns>
        private Stepper LoopStep()
        {
            this.expressions = Rest(this.expressions);
            return ContinueHere(this.EvalExprStep);
        }
        #endregion
    }
}