// <copyright file="EvaluateParallel.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a sequence of exprs in parallel by evaluating each member.
    /// </summary>
    public sealed class EvaluateParallel : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-parallel";

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
        /// Initializes a new instance of the EvaluateParallel class.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateParallel(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.expressions = expr;
            ContinueHere(EvalExprStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call the parallel evaluator.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The parallel evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateParallel(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Initial step: see if we are done.
        /// If not, evaluate the next expression.
        /// If we are, evaluate and return.
        /// If the expression suspended, then go on anyway with the next expr.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step.</returns>
        private static Stepper EvalExprStep(Stepper s)
        {
            EvaluateParallel step = (EvaluateParallel)s;
            if (EmptyList.Is(step.expressions))
            {
                return s.ReturnFromStep(Undefined.Instance);
            }

            Stepper res = EvaluateExpression.Call(List.First(step.expressions), s.Env, s.ContinueHere(LoopStep));
// TODO fix this
// If the result is suspended, then we must finish the evaluation but must NOT continue on to the next one.
// write a test for this and fix it
            return res.IsSuspended ? s.ContinueHere(LoopStep) : res;
        }

        /// <summary>
        /// Comes back here after expression evaluation.  Loop back and evaluate another.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Immediately steps back.</returns>
        private static Stepper LoopStep(Stepper s)
        {
            EvaluateParallel step = (EvaluateParallel)s;
            step.expressions = List.Rest(step.expressions);
            return s.ContinueHere(EvalExprStep);
        }
        #endregion
    }
}