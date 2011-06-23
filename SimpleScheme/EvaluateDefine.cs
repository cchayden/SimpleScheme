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
        /// Initializes a new instance of the EvaluateDefine class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateDefine(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.InitialStep;
            IncrementCounter("define");
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
            if (List.First(this.Expr) is Pair)
            {
                this.Pc = this.StoreStep1;
                return EvaluatorMain.Call(
                    this, List.Cons("lambda", List.Cons(List.Rest(List.First(this.Expr)), List.Rest(this.Expr))));
            }

            this.Pc = this.StoreStep2;
            return EvaluatorMain.Call(this, List.Second(this.Expr));
        }

        /// <summary>
        /// Back from evaluating the lambda.  Store the result in the environment
        /// </summary>
        /// <returns>Execution continues in the caller.</returns>
        private Stepper StoreStep1()
        {
            return ReturnFromStep(this.Env.Define(List.First(List.First(this.Expr)), ReturnedExpr));
        }

        /// <summary>
        /// Back from defun.  Define the name to be the evaluated expression.
        /// </summary>
        /// <returns>Execution continues in the caller.</returns>
        private Stepper StoreStep2()
        {
            return ReturnFromStep(this.Env.Define(List.First(this.Expr), ReturnedExpr));
        }
    }
}