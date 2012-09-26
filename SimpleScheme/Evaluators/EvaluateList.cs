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
    //// <r4rs section="6.3">(list <obj> ...)</r4rs>
    public sealed class EvaluateList : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-list");

        /// <summary>
        /// The result that will be returned.
        /// </summary>
        private SchemeObject result;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateList class.
        /// This is used in the "list" primitive, and ALSO to evaluate the
        ///   arguments in a list that is part of procedure application.
        /// Because it is used internally, the evaluator must not use destructive
        ///   operations on its member variables.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateList(SchemeObject expr, Environment env, Evaluator caller)
            : base(expr, env, caller, counter)
        {
            // Start with an empty list.  As exprs are evaluated, they will be consed on the
            //  front.  The list will be reversed before it is returned.  Do this rather than
            //  building a list in place so that the evaluator can be cloned.
            this.result = EmptyList.Instance;
            this.ContinueAt(EvalExprStep);
        }

        #endregion

        #region Public Static Methods
        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>A list evaluator.</returns>
        public static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            // first check for degenerate cases
            if (expr is EmptyList)
            {
                return caller.UpdateReturnValue(EmptyList.Instance);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad args for list: " + expr, null);
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            return new EvaluateList(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Create the list by evaluating the expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Next step evaluates the first expression.</returns>
        private static Evaluator EvalExprStep(Evaluator s)
        {
            // there is more to do --  evaluate the first expression
            return EvaluateExpression.Call(First(s.Expr), s.Env, s.ContinueAt(LoopStep));
        }

        /// <summary>
        /// Back from evaluating the expression.  Accumulate the result and, if there
        ///   is anything left, loop back to evaluate another expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The created list, or an evaluator to loop back and evaluate some more.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            var step = (EvaluateList)s;

            // back from the evaluation -- save the result and keep going with the rest
            step.result = Cons(EnsureSchemeObject(s.ReturnedExpr), step.result);
            step.StepDownExpr();

            if (s.Expr is Pair)
            {
                // Come back to this step, so don't assign PC for better performance.
                return EvaluateExpression.Call(First(s.Expr), s.Env, s);
            }

            // We are done.  Reverse the list and return it.
            if (step.result is EmptyList || Rest(step.result) is EmptyList)
            {
                return step.ReturnFromStep(step.result);
            }

            return step.ReturnFromStep(ReverseList(step.result));
        }
        #endregion
    }
}