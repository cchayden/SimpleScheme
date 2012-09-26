// <copyright file="EvaluateMap.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

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
            : base(OpCode.ApplyFun, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(proc != null);
            Contract.Requires(counter >= 0);
            //// result can be null if the caller does not need the result
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
            Contract.Requires(proc != null);
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);

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
        /// <returns>If apply recurses, return that evaluator.  Otherwise go on to save result.
        /// If we are done, return the collected results.</returns>
        protected override Evaluator ApplyFunStep()
        {
            if (First(this.Expr) is Pair)
            {
                // Grab the arguments to the applications (the head of each list).
                // Then the proc is applied to them.
                this.Pc = OpCode.CollectAndLoop;
                return this.proc.Apply(MapFun(First, this.Expr), this, this);
            }

            // if we are done, return the reversed result list
            // We cannot do this destructively without breaking call/cc.
            Evaluator caller = this.Caller;
            Contract.Assert(caller != null);
            caller.ReturnedExpr = this.result != null ? Pair.ReverseListInPlace(this.result) : Undefined.Instance;
            return caller;
        }

        /// <summary>
        /// Collect the result of the function application and loop back to do the next one.
        /// </summary>
        /// <returns>Continue back in apply fun step.</returns>
        protected override Evaluator CollectAndLoopStep()
        {
            // back from the evaluation -- save the result and keep going with the rest
            if (this.result != null)
            {
                // Builds a list by tacking new values onto the head.
                this.result = Cons(this.ReturnedExpr, this.result);
            }

            // Move down each of the lists
            this.Expr = MapFun(Rest, this.Expr);
            this.Pc = OpCode.ApplyFun;
            return this;
        }
        #endregion
    }
}
