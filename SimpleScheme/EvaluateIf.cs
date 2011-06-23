// <copyright file="EvaluateIf.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate an if expression.
    /// Evaluate the first part, then depending on its truth value, either
    ///   evaluate the second or third part.
    /// </summary>
    //// <r4rs section="4.1.5">(if <test> <consequent> <alternate>)</r4rs>
    //// <r4rs section="4.1.5">(if <test> <consequent>)</r4rs>
    public sealed class EvaluateIf : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateIf class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateIf(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            this.Pc = this.EvaluateTestStep;
            IncrementCounter("if");
        }

        /// <summary>
        /// Creates an if evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The if evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateIf(caller, expr, caller.Env);
        }

        /// <summary>
        /// Begin by evaluating the first expression (the test).
        /// </summary>
        /// <returns>Steps to evaluate the test.</returns>
        private Stepper EvaluateTestStep()
        {
            this.Pc = this.EvaluateAlternativeStep;
            return EvaluatorMain.Call(this, List.First(this.Expr));
        }

        /// <summary>
        /// Back here after the test has been evaluated.
        /// Evaluate and return either the second or third expression.
        /// If there is no thid, the empty list will be evaluated, which is OK.
        /// </summary>
        /// <returns>Execution continues with the caller.</returns>
        private Stepper EvaluateAlternativeStep()
        {
            return EvaluatorMain.Call(
                this.Parent, 
                SchemeBoolean.Truth(ReturnedExpr) ? List.Second(this.Expr) : List.Third(this.Expr));
        }
    }
}
