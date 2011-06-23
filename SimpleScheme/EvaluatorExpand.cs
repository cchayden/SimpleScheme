// <copyright file="EvaluatorExpand.cs" company="Charles Hayden">
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
        /// Expand a macro.
        /// </summary>
        private class EvaluatorExpand : Evaluator
        {
            /// <summary>
            /// The macro to expand.
            /// </summary>
            private readonly Macro fn;

            /// <summary>
            /// Initializes a new instance of the EvaluatorExpand class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            /// <param name="fn">The macro to expand.</param>
            public EvaluatorExpand(Scheme interp, Evaluator parent, object expr, Environment env, Macro fn)
                : base(interp, parent, expr, env)
            {
                this.fn = fn;
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
                        object expanded = this.fn.Expand(Interp, this, Expr);
                        if (expanded is Evaluator)
                        {
                            Pc = 1;
                            return SubCall((Evaluator)expanded);
                        }

                        return EvalError("Expand: should not get here");
                    case 1:
                        return SubReturn(ReturnedExpr);
                }

                return EvalError("Expand: program counter error");
            }
        }
    }
}