#define OLD
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
    internal sealed class EvaluateList : Evaluator
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
            : base(EvalExprStep, expr, env, caller, counter)
        {
            // Start with an empty list.  As exprs are evaluated, they will be consed on the
            //  front.  The list will be reversed before it is returned.  Because this is done
            //  destructively, cloning needs to copy the result.
            this.result = EmptyList.Instance;
        }
        #endregion

        #region Call
        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>A list evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            // first check for degenerate cases
            if (expr is EmptyList)
            {
                caller.ReturnedExpr = EmptyList.Instance;
                return caller;
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad args for list: " + expr, null);
            }

            if (AllEvaluated(expr))
            {
                // Skip creating an evaluator or a list if all elements are constants.
                caller.ReturnedExpr = expr;
                return caller;
            }

            if (AllSimple(expr))
            {
                // Skip creating an evaluator if all elements are constants or symbols.
                caller.ReturnedExpr = EvaluateSymbolsInList(expr, env);
                return caller;
            }

            // Something actually needs to be evaluated.
            return new EvaluateList(expr, env, caller);
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
            var copy = (EvaluateList)this.MemberwiseClone();
            //// also copy result field
            copy.result = Copy(this.result);
            return copy;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Tests if all the members of a list are self-evaluating.
        /// Forms that are NOT self-evaluating are pair and symbol.
        /// If nothing in the list actually needs to be evaluated, then no list needs
        ///   to be created.
        /// </summary>
        /// <param name="expr">The expression to be tested.</param>
        /// <returns>True if all list elements are self-evaluating.</returns>
        private static bool AllEvaluated(SchemeObject expr)
        {
            while (expr is Pair)
            {
                SchemeObject first = First(expr);
                if (first is Pair || first is Symbol)
                {
                    return false;
                }

                expr = Rest(expr);
            }

            return true;
        }

        /// <summary>
        /// Test if all items in a list are simple (non-pairs).
        /// If so, then we can just look up the symbols and return the rest.
        /// </summary>
        /// <param name="expr">The list to test.</param>
        /// <returns>True if all items are simple.</returns>
        private static bool AllSimple(SchemeObject expr)
        {
            while (expr is Pair)
            {
                SchemeObject first = First(expr);
                if (first is Pair)
                {
                    return false;
                }

                expr = Rest(expr);
            }
            return false;
        }

        /// <summary>
        /// Copy a list, evaluating the symbols.
        /// When used here, the list only contains smybols and self-evaluating objects.
        /// So by evaluating the symbols and collecting the results into a list, we can
        ///   avoid having to create an evaluator.
        /// </summary>
        /// <param name="expr">The list of items to copy or look up.</param>
        /// <param name="env">The environment for the lookup.</param>
        /// <returns>The listof results.</returns>
        private static SchemeObject EvaluateSymbolsInList(SchemeObject expr, Environment env)
        {
            return MapFun((obj) => (obj is Symbol ? env.Lookup((Symbol)obj) : obj), expr);
        }

        #endregion

        #region Steps
        /// <summary>
        /// Create the list by evaluating the expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Next step evaluates the first expression.</returns>
        private static Evaluator EvalExprStep(Evaluator s)
        {
            // this is executed only for the first expression
            s.Pc = LoopStep;
            return EvaluateExpression.Call(First(s.Expr), s.Env, s);
        }

        /// <summary>
        /// Back from evaluating the expression.  Accumulate the result and, if there
        ///   is anything left, loop back here to evaluate another expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The created list, or an evaluator to loop back and evaluate some more.</returns>
        private static Evaluator LoopStep(Evaluator s)
        {
            var step = (EvaluateList)s;

            // back from the evaluation -- save the result and keep going with the rest
            step.result = Cons(s.ReturnedExpr, step.result);
            step.Expr = Rest(step.Expr);

            if (s.Expr is Pair)
            {
                // Come back to this step, so don't assign PC for better performance.
                return EvaluateExpression.Call(First(s.Expr), s.Env, s);
            }

            // We are done.  Reverse the list and return it.
            Evaluator caller = step.Caller;
            if (Rest(step.result) is EmptyList)
            {
                // only one element -- no need to reverse
                caller.ReturnedExpr = step.result;
            }
            else
            {
                // We can only do this destructively if we have copied the result on Clone.
                caller.ReturnedExpr = Pair.ReverseListInPlace(step.result);
            }

            return caller;
        }

        #endregion
    }
}