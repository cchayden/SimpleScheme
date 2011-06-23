// <copyright file="EvaluatorIf.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate an if expression.
    /// Evaluate the first part, then depending on its truth value, either
    ///   evaluate the second or third part.
    /// </summary>
    public class EvaluatorIf : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorIf class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluatorIf(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Creates an if evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <returns>The if evaluator.</returns>
        public static EvaluatorIf New(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorIf(parent, expr, env);
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
                    return CallEval(First(this.Expr));

                case PC.Step1:
                    return SubReturn(Truth(ReturnedExpr) ? Second(this.Expr) : Third(this.Expr));
            }

            return EvalError("If: program counter error");
        }
    }
}
