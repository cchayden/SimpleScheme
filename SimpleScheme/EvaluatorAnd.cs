// <copyright file="EvaluatorAnd.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence by evaluating each member.
    /// If a value is #f then return it.  Otherwise return the last value.
    /// </summary>
    public class EvaluatorAnd : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorAnd class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluatorAnd(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <returns>The and evaluator.</returns>
        public static EvaluatorAnd New(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorAnd(parent, expr, env);
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