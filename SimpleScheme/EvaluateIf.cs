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
        }

        /// <summary>
        /// Creates an if evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The if evaluator.</returns>
        public static EvaluateIf Call(Stepper caller, object expr)
        {
            return new EvaluateIf(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluate an if expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            switch (Pc)
            {
                case PC.Initial:
                    Pc = PC.Step1;
                    return EvaluatorMain.Call(this, List.First(this.Expr));

                case PC.Step1:
                    return ReturnFromStep(SchemeBoolean.Truth(ReturnedExpr) ? List.Second(this.Expr) : List.Third(this.Expr));
            }

            return ErrorHandlers.EvalError("If: program counter error");
        }
    }
}
