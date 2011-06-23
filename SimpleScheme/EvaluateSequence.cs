// <copyright file="EvaluateSequence.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence by evaluating each member and returning the last value.
    /// </summary>
   //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
    public sealed class EvaluateSequence : Stepper
    {
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
        private object expressions;

        /// <summary>
        /// Initializes a new instance of the EvaluateSequence class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateSequence(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
            this.expressions = expr;
            ContinueHere(this.EvalExprStep);
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
        }

        /// <summary>
        /// Call the sequence evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The sequence evaluator.</returns>
        public static Stepper Call(object expr, Environment env, Stepper caller)
        {
            return new EvaluateSequence(expr, env, caller);
        }

        /// <summary>
        /// Call the sequence evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The sequence evaluator.</returns>
        public static Stepper Call(object expr, Stepper caller)
        {
            return new EvaluateSequence(expr, caller.Env, caller);
        }

        /// <summary>
        /// Initial step: to see if we are done.
        /// If not, evaluate the next expression.
        /// If we are, evaluate and return the last expr.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalExprStep()
        {
            var nextStep = Rest(this.expressions) == List.Empty ? (StepperFunction)this.ReturnStep : this.LoopStep;
            return EvaluateExpression.Call(First(this.expressions), ContinueHere(nextStep));
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
    }
}