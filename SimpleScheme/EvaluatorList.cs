// <copyright file="EvaluatorList.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluator contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate the items in a list, given the environment.
        /// This is done to the args of a procedure call (except for special forms).
        /// This is an iterative, rather than a recursive one.
        /// </summary>
        private class EvaluatorList : Stepper
        {
            /// <summary>
            /// The result that will be returned.
            /// </summary>
            private readonly Pair result;

            /// <summary>
            /// The end of the list we are constructing for return.
            /// </summary>
            private Pair accum;

            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorList class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorList(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
                // start with an empty list
                // the empty cell will be stripped off at the end
                this.accum = this.result = List(null); 
            }

            /// <summary>
            /// Evaluate a list of expressions.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper EvalStep()
            {
                switch (this.Pc)
                {
                    case 0:
                        // first check for degenerate cases
                        if (this.Expr == null)
                        {
                            return SubReturn(null);
                        }

                        if (!(this.Expr is Pair))
                        {
                            Error("Illegal arg list: " + this.Expr);
                            return SubReturn(null);
                        }

                        Pc = 1;
                        return SubContinue();

                    case 1:
                        if (this.Expr is Pair)
                        {
                            // if there is more to do, evaluate the first expression
                            Pc = 2;
                            return CallEval(First(this.Expr));
                        }

                        // if we are done, just return the result minus the dummy entry
                        return this.SubReturn(this.result.Rest);

                    case 2:
                        // back from the evaluation -- save the result and keep going with the rest
                        Pc = 1;
                        this.accum = (Pair)(this.accum.Rest = List(ReturnedExpr));
                        Expr = Rest(Expr);
                        return SubContinue();
                }

                return EvalError("List: program counter error");
            }
        }
    }
}