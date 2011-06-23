// <copyright file="EvaluatorDefine.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    public class EvaluatorDefine : Evaluator
    {
        /// <summary>
        /// Initializes a new instance of the EvaluatorDefine class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorDefine(Scheme interp, Evaluator parent, object expr, Environment env) 
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a define expression.
        /// Handle the define syntax shortcut by inserting lambda.
        /// </summary>
        /// <returns>The evaluated definition.</returns>
        public override Evaluator EvalStep()
        {
            switch (Pc)
            {
                case 0:
                    if (First(this.Expr) is Pair)
                    {
                        Pc = 1;
                        return CallEval(Cons("lambda", Cons(Rest(First(this.Expr)), Rest(this.Expr))));
                    }
                    Pc = 2;
                    return CallEval(Second(this.Expr));
                case 1:
                    this.RetExpr = this.Env.Define(First(First(this.Expr)), Called.RetExpr);
                    break;
                case 2:
                    this.RetExpr = this.Env.Define(First(this.Expr), Called.RetExpr);
                    break;
            }
            return EvalReturn();
        }
    }
}