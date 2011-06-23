// <copyright file="EvaluateDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    public sealed class EvaluateDefine : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "define";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// Initializes a new instance of the EvaluateDefine class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateDefine(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
            ContinueHere(this.InitialStep);
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
        /// Call a define evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The define evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateDefine(caller, expr, caller.Env);
        }

        /// <summary>
        /// Handle the two forms of define.
        /// In the first case, just rewrite as a lambda and evaluate that.
        /// In the second case (defun) start by evaluating the body.
        /// </summary>
        /// <returns>Continue by evaluating the body of the definition.</returns>
        private Stepper InitialStep()
        {
            if (First(Expr) is Pair)
            {
                // TODO rewrite
                return EvaluateExpression.Call(
                    ContinueHere(this.StoreStep1), Cons("lambda", Cons(Rest(First(Expr)), Rest(Expr))));
            }

            return EvaluateExpression.Call(ContinueHere(this.StoreStep2), Second(Expr));
        }

        /// <summary>
        /// Back from evaluating the lambda.  Store the result in the environment
        /// </summary>
        /// <returns>Execution continues in the caller.</returns>
        private Stepper StoreStep1()
        {
            return ReturnFromStep(this.Env.Define(First(First(Expr)), ReturnedExpr));
        }

        /// <summary>
        /// Back from defun.  Define the name to be the evaluated expression.
        /// </summary>
        /// <returns>Execution continues in the caller.</returns>
        private Stepper StoreStep2()
        {
            return ReturnFromStep(this.Env.Define(First(Expr), ReturnedExpr));
        }
    }
}