// <copyright file="EvaluatorAnd.cs" company="Charles Hayden">
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
        /// Evaluate a sequence by evaluating each member.
        /// If a value is #f then return it.  Otherwise return the last value.
        /// </summary>
        private class EvaluatorAnd : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorAnd class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorAnd(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate a sequence of objects, returning  when one is #f, or returning the last.
            /// </summary>
            /// <returns>The next step.</returns>
            public override Stepper RunStep()
            {
                while (true)
                {
                    switch (this.Pc)
                    {
                        case PC.Initial:
                            if (this.Expr == null)
                            {
                                return SubReturn(True);
                            }

                            this.Pc = PC.Step1;
                            continue;

                        case PC.Step1:
                            this.Pc = Rest(this.Expr) == null ? PC.Step3 : PC.Step2;
                            return CallEval(First(this.Expr));

                        case PC.Step2:
                            if (IsFalse(this.ReturnedExpr))
                            {
                                return SubReturn(False);
                            }

                            this.Expr = Rest(this.Expr);
                            this.Pc = PC.Step1;
                            continue;

                        case PC.Step3:
                            return SubReturn(this.ReturnedExpr);
                    }

                    return EvalError("And: program counter error");
                }
            }
        }
    }
}