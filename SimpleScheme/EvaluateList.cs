// <copyright file="EvaluateList.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate the items in a list, given the environment.
    /// This is done to the args of a procedure call (except for special forms).
    /// This is an iterative, rather than a recursive one.
    /// </summary>
    public sealed class EvaluateList : Stepper
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
        /// Initializes a new instance of the EvaluateList class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateList(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            // start with an empty list
            // the empty cell will be stripped off at the end
            this.accum = this.result = List.MakeList(null);
        }

        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>A list evaluator.</returns>
        public static EvaluateList Call(Stepper caller, object expr)
        {
            return new EvaluateList(caller, expr, caller.Env);
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
                            return ReturnFromStep(null);
                        }

                        if (!(this.Expr is Pair))
                        {
                            ErrorHandlers.Error("Illegal arg list: " + this.Expr);
                            return ReturnFromStep(null);
                        }

                        Pc = PC.Step1;
                        continue;

                    case PC.Step1:
                        // there is more to do --  evaluate the first expression left
                        Pc = PC.Step2;
                        return EvaluatorMain.Call(this, List.First(this.Expr));

                    case PC.Step2:
                        // back from the evaluation -- save the result and keep going with the rest
                        Pc = PC.Step1;
                        this.accum = (Pair)(this.accum.Rest = List.MakeList(ReturnedExpr));
                        Expr = List.Rest(Expr);

                        // if we are done now, return
                        if (!(Expr is Pair))
                        {
                            return ReturnFromStep(this.result.Rest);
                        }

                        continue;
                }

                return ErrorHandlers.EvalError("List: program counter error");
            }
        }
    }
}