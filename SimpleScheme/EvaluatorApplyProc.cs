﻿// <copyright file="EvaluatorApplyProc.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>

namespace SimpleScheme
{
    /// <summary>
    /// Evaluator contains all the individual evaluators
    /// </summary>
    public partial class Evaluator
    {
        /// <summary>
        /// Evaluate args and apply a proc to it.
        /// </summary>
        private class EvaluatorApplyProc : Evaluator
        {
            /// <summary>
            /// The proc or primitive to apply.
            /// </summary>
            private readonly object fn;

            /// <summary>
            /// Initializes a new instance of the EvaluatorApplyProc class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            /// <param name="fn">The function to apply.</param>
            public EvaluatorApplyProc(Scheme interp, Evaluator parent, object expr, Environment env, object fn)
                : base(interp, parent, expr, env)
            {
                this.fn = fn;
            }

            /// <summary>
            /// Evaluate a proc application.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (this.Pc)
                {
                    case 0:
                        Pc = 1;
                        return CallList(Expr);
                    case 1:
                        object res = Procedure.Proc(this.fn).Apply(this.Interp, this, ReturnedExpr);
                        if (res is Evaluator)
                        {
                            Pc = 2;
                            return this.SubCall((Evaluator)res);
                        }

                        return SubReturn(res);
                    case 2:
                        return SubReturn(ReturnedExpr);
                }

                return EvalError("ApplyProc: program counter error");
            }
        }
    }
}