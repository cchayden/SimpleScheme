// <copyright file="EvaluateSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a set! expression.
    /// </summary>
    public sealed class EvaluateSet : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateSet class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateSet(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Calls a set evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The set evaluator.</returns>
        public static EvaluateSet Call(Stepper caller, object expr)
        {
            return new EvaluateSet(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            switch (Pc)
            {
                case PC.Initial:
                    Pc = PC.Step1;
                    return EvaluatorMain.Call(this, List.Second(this.Expr));

                case PC.Step1:
                    return ReturnFromStep(this.Env.Set(List.First(this.Expr), ReturnedExpr));
            }

            return ErrorHandlers.EvalError("Set: program counter error");
        }
    }
}