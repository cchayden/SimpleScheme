// <copyright file="EvaluatorEvalExpand.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    public partial class Evaluator
    {
        private class EvaluatorEvalExpand : Evaluator
        {
            /// <summary>
            /// Initializes a new instance of the EvaluatorEvalExpand class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorEvalExpand(Scheme interp, Evaluator parent, object expr, Environment env)
                : base(interp, parent, expr, env) { }

            /// <summary>
            /// Expand a macro
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (Pc)
                {
                    case 0:
                        Pc = 1;
                        return CallEvalGlobal(First(this.Expr));
                    case 1:
                        object fn = ReturnedExpr;
                        if (!(fn is Macro))
                        {
                            return SubReturn(this.Expr);
                        }
                        object expanded = ((Macro) fn).Expand(Interp, Parent, (Pair)this.Expr, Rest(this.Expr));
                        return SubReturn(expanded);
                }
                throw new Exception("program counter error");
            }

        }
    }

    public partial class Evaluator
    {
        private class EvaluatorExpand : Evaluator
        {
            private object fn;
            private object args;

            /// <summary>
            /// Initializes a new instance of the EvaluatorEvalExpand class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorExpand(Scheme interp, Evaluator parent, object expr, Environment env, object fn, object args)
                : base(interp, parent, expr, env)
            {
                this.fn = fn;
                this.args = args;
            }

            /// <summary>
            /// Expand a macro
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Evaluator EvalStep()
            {
                switch (Pc)
                {
                    case 0:
                        object expanded = ((Macro) fn).Expand(Interp, this, (Pair)this.Expr, args);
                        if (expanded is Evaluator)
                        {
                            Pc = 1;
                            return SubCall((Evaluator)expanded);
                        }
                        throw new Exception("should not get here");
                        return SubReturn(expanded);
                    case 1:
                        return SubReturn(ReturnedExpr);
                }
                throw new Exception("program counter error");
            }

        }
    }
}