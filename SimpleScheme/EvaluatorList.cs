// <copyright file="EvaluatorList.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate the items in a list, given the environment.
    /// This is done to the args of a procedure call (except for special forms).
    /// This is an iterative, rather than a recursive one.
    /// </summary>
    public class EvaluatorList : Stepper
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
        /// Initializes a new instance of the EvaluatorList class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluatorList(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            // start with an empty list
            // the empty cell will be stripped off at the end
            this.accum = this.result = List(null);
        }

        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <returns>A list evaluator.</returns>
        public static EvaluatorList New(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorList(parent, expr, env);
        }

        /// <summary>
        /// Evaluate a list of expressions.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            while (true)
            {
                switch (this.Pc)
                {
                    case PC.Initial:
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

                        Pc = PC.Step1;
                        continue;

                    case PC.Step1:
                        // there is more to do --  evaluate the first expression left
                        Pc = PC.Step2;
                        return CallEval(First(this.Expr));

                    case PC.Step2:
                        // back from the evaluation -- save the result and keep going with the rest
                        Pc = PC.Step1;
                        this.accum = (Pair)(this.accum.Rest = List(ReturnedExpr));
                        Expr = Rest(Expr);

                        // if we are done now, return
                        if (!(Expr is Pair))
                        {
                            return SubReturn(this.result.Rest);
                        }

                        continue;
                }

                return EvalError("List: program counter error");
            }
        }
    }
}