// <copyright file="EvaluateSequence.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence by evaluating each member and returning the last value.
    /// </summary>
    public sealed class EvaluateSequence : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateSequence class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateSequence(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.EvalExprStep;
            IncrementCounter("sequence");
        }

        /// <summary>
        /// Call the sequence evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The sequence evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateSequence(caller, expr, caller.Env);
        }

        /// <summary>
        /// Initial step: to see if we are done.
        /// If not, evaluate the next expression.
        /// If we are, evaluate and return the last expr.
        /// </summary>
        /// <returns>The next step.</returns>
        private Stepper EvalExprStep()
        {
            if (List.Rest(this.Expr) == null)
            {
                this.Pc = this.ReturnStep;
                return EvaluatorMain.Call(this, List.First(this.Expr));
            }

            this.Pc = this.LoopStep;
            return EvaluatorMain.Call(this, List.First(this.Expr));
        }

        /// <summary>
        /// Comes back here after expression evaluation.  Loop back and evaluate another
        /// </summary>
        /// <returns>Immediately steps back.</returns>
        private Stepper LoopStep()
        {
            this.Pc = this.EvalExprStep;
            return this.LoopStep(List.Rest(this.Expr));
        }
    }
}