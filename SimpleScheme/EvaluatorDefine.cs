// <copyright file="EvaluatorDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate a define expression.
        /// </summary>
        private class EvaluatorDefine : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorDefine class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorDefine(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
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
}