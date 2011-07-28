// <copyright file="EvaluateList.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate the items in a list, given the environment.
    /// This is done to the args of a procedure call (except for special forms).
    /// This is an iterative, rather than a recursive one.
    /// </summary>
    //// <r4rs section="6.3">(list <obj> ...)</r4rs>
    internal sealed class EvaluateList : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        internal const string StepperName = "evaluate-list";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The result that will be returned.
        /// </summary>
        private Obj result;

        /// <summary>
        /// Objs to make into a list.
        /// </summary>
        private Obj objs;
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
        private EvaluateList(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.objs = expr;

            // Start with an empty list.  As exprs are evaluated, they will be consed on the
            //  front.  The list will be reversed before it is returned.  Do this rather than
            //  building a list in place so that the evaluator can be cloned.
            this.result = EmptyList.Instance;
            ContinueHere(EvalExprStep);
            IncrementCounter(counter);
        }

        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>A list evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            // first check for degenerate cases
            if (EmptyList.IsEmptyList(expr))
            {
                return caller.ContinueStep(EmptyList.Instance);
            }

            if (!Pair.IsPair(expr))
            {
                ErrorHandlers.SemanticError("Bad args for list: " + expr);
                return caller.ContinueStep(Undefined.Instance);
            }

            return new EvaluateList(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Create the list by evaluating the expression.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Next step evaluates the first expression.</returns>
        private static Stepper EvalExprStep(Stepper s)
        {
            EvaluateList step = (EvaluateList)s;

            // there is more to do --  evaluate the first expression
            return EvaluateExpression.Call(List.First(step.objs), s.Env, s.ContinueHere(LoopStep));
        }

        /// <summary>
        /// Back from evaluating the expression.  Accumulate the result and, if there
        ///   is anything left, loop back to evaluate another expression.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The created list, or a stepper to loop back and evaluate some more.</returns>
        private static Stepper LoopStep(Stepper s)
        {
            EvaluateList step = (EvaluateList)s;

            // back from the evaluation -- save the result and keep going with the rest
            step.result = List.Cons(s.ReturnedExpr, step.result);
            step.objs = List.Rest(step.objs);

            if (Pair.IsPair(step.objs))
            {
                // Come back to this step, so don't assign PC for better performance.
                return EvaluateExpression.Call(List.First(step.objs), s.Env, s);
            }

            // We are done.  Reverse the list and return it.
            if (EmptyList.IsEmptyList(step.result) || EmptyList.IsEmptyList(List.Rest(step.result)))
            {
                return s.ReturnFromStep(step.result);
            }

            return s.ReturnFromStep(List.Reverse(step.result));
        }
        #endregion
    }
}