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
            this.Pc = this.InitialStep;
            IncrementCounter("list");
        }

        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>A list evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateList(caller, expr, caller.Env);
        }
        
        /// <summary>
        /// Start by testing for an empty or an illegal argument.
        /// </summary>
        /// <returns>Result if empty or illegal, otherwise null to continue to next step.</returns>
        private Stepper InitialStep()
        {
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

            this.Pc = this.EvalExprStep;
            return this;
        }

        /// <summary>
        /// Create the list by evaluating the expression.
        /// </summary>
        /// <returns>Next step evaluates the first expression.</returns>
        private Stepper EvalExprStep()
        {
            // there is more to do --  evaluate the first expression
            this.Pc = this.LoopStep;
            return EvaluatorMain.Call(this, List.First(this.Expr));
        }

        /// <summary>
        /// Back from evaluating the expression.  Accumulate the result and, if there
        ///   is anything left, loop back to evaluate another expression.
        /// </summary>
        /// <returns>The created list, or null to loop back and evaluate some more.</returns>
        private Stepper LoopStep()
        {
            // back from the evaluation -- save the result and keep going with the rest
            this.Pc = this.EvalExprStep;
            this.accum = (Pair)(this.accum.Rest = List.MakeList(ReturnedExpr));
            this.LoopStep(List.Rest(Expr));

            // if we are done now, return
            if (!(Expr is Pair))
            {
                return ReturnFromStep(this.result.Rest);
            }

            return this;
        }
    }
}