// <copyright file="EvaluatorList.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate the items in a list, given the environment.
    /// This is done to the args of a procedure call (except for special forms).
    /// This is an iterative, rather than a recursive one.
    /// </summary>
    public class EvaluatorList : Evaluator
    {
        private Pair result;
        private Pair accum;

        /// <summary>
        /// Initializes a new instance of the EvaluatorList class.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        public EvaluatorList(Scheme interp, Evaluator parent, object expr, Environment env)
            : base(interp, parent, expr, env) { }

        /// <summary>
        /// Evaluate a list of expressions.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Evaluator EvalStep()
        {
            switch (this.Pc)
            {
                case 0:
                    // first check for degenerate cases
                    if (this.Expr == null)
                    {
                        this.RetExpr = null;
                        break;
                    }

                    if (!(this.Expr is Pair))
                    {
                        Error("Illegal arg list: " + this.Expr);
                        this.RetExpr = null;
                        break;
                    }
                    // start with an empty list
                    accum = result = List(null); // empty cell will be stripped off below
                    Pc = 1;
                    return EvalContinue();
                case 1:
                    if (this.Expr is Pair)
                    {
                        // if there is more to do, evaluate the first expression
                        Pc = 2;
                        return CallEval(First(this.Expr));
                    }
                    // if we are done, just return the result minus the dummy entry
                    this.RetExpr = result.Rest;
                    break;
                case 2:
                    // back from the evaluation -- save the result and keep going with the rest
                    Pc = 1;
                    accum = (Pair) (accum.Rest = List(Called.RetExpr));
                    Expr = Rest(Expr);
                    return EvalContinue();
            }
            return EvalReturn();
        }
    }
}