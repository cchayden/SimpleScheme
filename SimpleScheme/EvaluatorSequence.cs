// <copyright file="EvaluatorSequence.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    public class EvaluatorSequence : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorSequence class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorSequence(Scheme interp, Evaluator parent, object expr, Environment env)
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a sequence of objects, returning the last.
        /// This was a simple while loop that has been split in the middle.
        /// </summary>
        /// <returns>The next step.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    if (Rest(this.Expr) != null)
                    {
                        this.Pc = 1;
                        return CallEval(First(this.Expr));
                    }
                    this.RetExpr = First(this.Expr);
                    break;
                case 1:
                    this.Expr = Rest(this.Expr);
                    this.Pc = 0;
                    return EvalContinue();
            }
            return EvalReturn();
        }
    }
}