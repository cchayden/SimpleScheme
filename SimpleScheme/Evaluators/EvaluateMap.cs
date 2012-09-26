// <copyright file="EvaluateMap.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate the items in a list, given the environment.
    /// This is done to the args of a procedure call (except for special forms).
    /// This is an iterative, rather than a recursive one.
    /// </summary>
    //// <r4rs section="6.9">(map proc <list1> <list2> ...)</r4rs>
    public sealed class EvaluateMap : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-map");

        /// <summary>
        /// The proc to apply to each element of the list.
        /// </summary>
        private readonly Procedure proc;

        /// <summary>
        /// Accumulates the returned result.
        /// The list is constructed in reverse order.
        /// This happens only if returnResult is true;
        /// </summary>
        private SchemeObject result;
        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the EvaluateMap class.
        /// This is used in the "map" primitive, and as part of "for-each".
        /// Because it is used internally, the evaluator must not use destructive
        ///   operations on its member variables.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="proc">The proc to apply to each element of the list.</param>
        /// <param name="result">Accumulate the result here, if not null.</param>
        private EvaluateMap(SchemeObject expr, Environment env, Evaluator caller, Procedure proc, SchemeObject result)
            : base(expr, env, caller)
        {
            this.proc = proc;
            this.result = result;

            this.ContinueHere(ApplyFunStep);
            this.IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call the map evaluator
        /// </summary>
        /// <param name="proc">The map proc.</param>
        /// <param name="expr">The map list to traverse.</param>
        /// <param name="returnResult">If true, return the result of the map.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller -- return to this when done.</param>
        /// <returns>The evaluator to execute.</returns>
        public static Evaluator Call(Procedure proc, SchemeObject expr, bool returnResult, Environment env, Evaluator caller)
        {
            // first check for degenerate cases
            if (expr is EmptyList)
            {
                return caller.UpdateReturnValue(EmptyList.Instance);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad args for map: " + expr);
                return caller.UpdateReturnValue(EmptyList.Instance);
            }

            SchemeObject result = returnResult ? EmptyList.Instance : null;
            return new EvaluateMap(expr, env, caller, proc, result);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Apply the map function to an element of the list and grab the result.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>If apply recurses, return that evaluator.  Otherwise go on to save result.
        /// If we are done, return the collected results.</returns>
        private static Evaluator ApplyFunStep(Evaluator s)
        {
            var step = (EvaluateMap)s;
            if (First(s.Expr) is Pair)
            {
                // Grab the arguments to the applications (the head of each list).
                // Then the proc is applied to them.
                return step.proc.Apply(MapFun(First, MakeList(s.Expr)), s.ContinueHere(CollectAndLoopStep));
            }

            // if we are done, return the reversed result list
            return s.ReturnFromStep(step.result != null ? ReverseList(step.result) : Undefined.Instance);
        }

        /// <summary>
        /// Collect the result of the function application and loop back to do the next one.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continue back in apply fun step.</returns>
        private static Evaluator CollectAndLoopStep(Evaluator s)
        {
            var step = (EvaluateMap)s;

            // back from the evaluation -- save the result and keep going with the rest
            if (step.result != null)
            {
                // Builds a list by tacking new values onto the head.
                step.result = Cons(s.ReturnedExpr, step.result);
            }

            // Move down each of the lists
            step.UpdateExpr(MapFun(Rest, MakeList(s.Expr)));
            return s.ContinueHere(ApplyFunStep);
        }
        #endregion
    }
}
