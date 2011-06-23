// <copyright file="EvaluateOr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a sequence of clauses by evaluating each member.
    /// If a value is not #f then return it.  Otherwise return the last value.
    /// </summary>
    public sealed class EvaluateOr : Stepper
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateOr class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateOr(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Creates a new or evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <returns>The or evaluator.</returns>
        public static EvaluateOr New(object expr, Environment env, Stepper parent)
        {
            return new EvaluateOr(parent, expr, env);
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
                            return ReturnFromStep(SchemeBoolean.False);
                        }

                        this.Pc = PC.Step1;
                        continue;

                    case PC.Step1:
                        this.Pc = List.Rest(this.Expr) == null ? PC.Step3 : PC.Step2;
                        return CallEval(List.First(this.Expr));

                    case PC.Step2:
                        if (SchemeBoolean.Truth(this.ReturnedExpr))
                        {
                            return ReturnFromStep(this.ReturnedExpr);
                        }

                        this.Expr = List.Rest(this.Expr);
                        this.Pc = PC.Step1;
                        continue;

                    case PC.Step3:
                        return ReturnFromStep(this.ReturnedExpr);
                }

                return ErrorHandlers.EvalError("Or: program counter error");
            }
        }
    }
}