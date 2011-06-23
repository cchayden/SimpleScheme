// <copyright file="EvaluatorApplyProc.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public class EvaluatorApplyProc : Evaluator
    {
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
                    Called = new EvaluatorList(Interp, this, Expr, Env);
                    return Called;
                case 1:
                    this.RetExpr = Procedure.Proc(fn).Apply(this.Interp, Called.RetExpr);
                    break;
            }
            return EvalReturn();
        }
        
    }
}