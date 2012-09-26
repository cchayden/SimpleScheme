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
    internal sealed class EvaluateMap : Evaluator
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
            : base(ApplyFunStep, expr, env, caller, counter)
        {
            this.proc = proc;
            this.result = result;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call the map evaluator
        /// </summary>
        /// <param name="proc">The map proc.</param>
        /// <param name="expr">The map list to traverse.</param>
        /// <param name="returnResult">If true, return the result of the map.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller -- return to this when done.</param>
        /// <returns>The evaluator to execute.</returns>
        internal static Evaluator Call(Procedure proc, SchemeObject expr, bool returnResult, Environment env, Evaluator caller)
        {
            // first check for degenerate cases
            if (expr is EmptyList)
            {
                caller.ReturnedExpr = EmptyList.Instance;
                return caller;
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad args for map: " + expr, null);
            }

            SchemeObject result = returnResult ? EmptyList.Instance : null;
            return new EvaluateMap(expr, env, caller, proc, result);
        }
        #endregion

        #region Clone
        /// <summary>
        /// Clone the evaluator when we need to store a continuation.
        /// Because we destructively modify result, we need to copy it as well.
        /// </summary>
        /// <returns>The cloned evaluator.</returns>
        internal override Evaluator Clone()
        {
            var copy = (EvaluateMap)this.MemberwiseClone();
            //// also copy result field
            copy.result = Copy(this.result);
            return copy;
        }
        #endregion

        #region Steps
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
                s.Pc = CollectAndLoopStep;
                return step.proc.Apply(MapFun(First, s.Expr), null, s, s);
            }

            // if we are done, return the reversed result list
            // We cannot do this destructively without breaking call/cc.
            Evaluator caller = step.Caller;
            caller.ReturnedExpr = step.result != null ? Pair.ReverseListInPlace(step.result) : Undefined.Instance;
            return caller;
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
            step.Expr = MapFun(Rest, s.Expr);
            s.Pc = ApplyFunStep;
            return s;
        }
        #endregion
    }
}
