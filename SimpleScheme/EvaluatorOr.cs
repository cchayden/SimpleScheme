// <copyright file="EvaluatorOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    public class EvaluatorOr : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorOr class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluatorOr(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Creates a new or evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <returns>The or evaluator.</returns>
        public static EvaluatorOr New(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorOr(parent, expr, env);
        }

        /// <summary>
        /// Evaluate a sequence of objects, returning the first not #f, or returning the last.
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
                            return SubReturn(False);
                        }

                        this.Pc = PC.Step1;
                        continue;

                    case PC.Step1:
                        this.Pc = Rest(this.Expr) == null ? PC.Step3 : PC.Step2;
                        return CallEval(First(this.Expr));

                    case PC.Step2:
                        if (Truth(this.ReturnedExpr))
                        {
                            return SubReturn(this.ReturnedExpr);
                        }

                        this.Expr = Rest(this.Expr);
                        this.Pc = PC.Step1;
                        continue;

                    case PC.Step3:
                        return SubReturn(this.ReturnedExpr);
                }

                return EvalError("Or: program counter error");
            }
        }
    }
}