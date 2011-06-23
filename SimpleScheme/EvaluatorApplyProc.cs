﻿// <copyright file="EvaluatorApplyProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public class EvaluatorApplyProc : Stepper
    {
        /// <summary>
        /// The proc or primitive to apply.
        /// </summary>
        private readonly object fn;

        /// <summary>
        /// Initializes a new instance of the EvaluatorApplyProc class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The function to apply.</param>
        private EvaluatorApplyProc(Stepper parent, object expr, Environment env, object fn)
            : base(parent, expr, env)
        {
            this.fn = fn;
        }

        /// <summary>
        /// Creates a new apply proc evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The function to apply.</param>
        /// <returns>The apply proc evaluator.</returns>
        public static EvaluatorApplyProc New(Stepper parent, object expr, Environment env, object fn)
        {
            return new EvaluatorApplyProc(parent, expr, env, fn);
        }

        /// <summary>
        /// Evaluate a proc application.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            while (true)
            {
                switch (this.Pc)
                {
                    case PC.Initial:
                        Pc = PC.Step1;
                        return CallList(Expr);

                    case PC.Step1:
                        object res = Procedure.Proc(this.fn).Apply(this, ReturnedExpr);
                        if (res is Stepper)
                        {
                            return this.GoToStep(PC.Step2, (Stepper)res);
                        }

                        return SubReturn(res);
                    case PC.Step2:
                        return SubReturn(ReturnedExpr);
                }

                return EvalError("ApplyProc: program counter error");
            }
        }
    }
}