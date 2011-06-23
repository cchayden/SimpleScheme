// <copyright file="EvaluatorMap.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate the items in a list, given the environment.
        /// This is done to the args of a procedure call (except for special forms).
        /// This is an iterative, rather than a recursive one.
        /// </summary>
        private class EvaluatorMap : Stepper
        {
            /// <summary>
            /// The proc to apply to each element of the list.
            /// </summary>
            private readonly Procedure proc;

            /// <summary>
            /// The initial value of the result.
            /// This is either the empty list, or null.
            /// If null, then no result is kept.
            /// </summary>
            private readonly Pair result;

            /// <summary>
            /// Accumulates the returned result.
            /// The is the end of the list that we are constructing.
            /// </summary>
            private Pair accum;

            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorMap class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            /// <param name="proc">The proc to apply to each element of the list.</param>
            /// <param name="result">The result is appended to this list.</param>
            public EvaluatorMap(Scheme interp, Stepper parent, object expr, Environment env, Procedure proc, Pair result)
                : base(interp, parent, expr, env)
            {
                this.proc = proc;
                this.result = result;
                this.accum = result;
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
                        if (First(this.Expr) is Pair)
                        {
                            Pc = 2;

                            // Grab the arguments to the applications (the head of each list).
                            // The the proc is applied to them.
                            object x = this.proc.Apply(Interp, this, MapFun(First, List(Expr)));
                            if (x is Stepper)
                            {
                                return SubCall((Stepper)x);
                            }

                            RetExpr = x;
                            return SubCall(this);
                        }

                        // if we are done, just return the result minus the dummy entry
                        if (this.result == null)
                        {
                            return SubReturn(null);
                        }

                        return SubReturn(this.result.Rest);

                    case 2:
                        // back from the evaluation -- save the result and keep going with the rest
                        Pc = 1;
                        if (this.result != null)
                        {
                            // Builds a list by tacking new values onto the tail.
                            this.accum = (Pair)(this.accum.Rest = List(ReturnedExpr));
                        }

                        // Step down each of the lists
                        Expr = MapFun(Rest, List(Expr));
                        return SubContinue();
                }

                return EvalError("Map: program counter error");
            }

            /// <summary>
            /// Traverse the given list, applying the given function to all elements.
            /// This is purely iterative.
            /// </summary>
            /// <param name="fun">The function to apply to each elment.</param>
            /// <param name="expr">The list to process.</param>
            /// <returns>A list made up of the function results of each input element.</returns>
            private static Pair MapFun(Func<object, object> fun, object expr)
            {
                Pair result = List(null);
                Pair accum = result;

                // Iterate down the list, taking the function and building a list of the results.
                expr = First(expr);
                while (expr is Pair)
                {
                    // Builds a list by tacking new values onto the tail.
                    accum = (Pair)(accum.Rest = List(fun(First(expr))));
                    expr = Rest(expr);
                }

                return (Pair)Rest(result);
            }
        }
    }
}
