// <copyright file="EvaluatorDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    public class EvaluatorDefine : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorDefine class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluatorDefine(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Create a define evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <returns>The define evaluator.</returns>
        public static EvaluatorDefine New(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorDefine(parent, expr, env);
        }

        /// <summary>
        /// Evaluate a define expression.
        /// Handle the define syntax shortcut by inserting lambda.
        /// </summary>
        /// <returns>The evaluated definition.</returns>
        public override Stepper RunStep()
        {
            switch (Pc)
            {
                case PC.Initial:
                    if (First(this.Expr) is Pair)
                    {
                        Pc = PC.Step1;
                        return CallEval(Cons("lambda", Cons(Rest(First(this.Expr)), Rest(this.Expr))));
                    }

                    Pc = PC.Step2;
                    return CallEval(Second(this.Expr));

                case PC.Step1:
                    return SubReturn(this.Env.Define(First(First(this.Expr)), ReturnedExpr));

                case PC.Step2:
                    return SubReturn(this.Env.Define(First(this.Expr), ReturnedExpr));
            }

            return EvalError("Define: program counter error");
        }
    }
}