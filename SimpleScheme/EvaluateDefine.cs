// <copyright file="EvaluateDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    public class EvaluateDefine : Stepper
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
        }

        /// <summary>
        /// Create a define evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <returns>The define evaluator.</returns>
        public static EvaluateDefine New(object expr, Environment env, Stepper parent)
        {
            return new EvaluateDefine(parent, expr, env);
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
                    return ReturnFromStep(this.Env.Define(First(First(this.Expr)), ReturnedExpr));

                case PC.Step2:
                    return ReturnFromStep(this.Env.Define(First(this.Expr), ReturnedExpr));
            }

            return EvalError("Define: program counter error");
        }
    }
}