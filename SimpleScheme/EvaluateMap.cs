// <copyright file="EvaluateMap.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Evaluate the items in a list, given the environment.
    /// This is done to the args of a procedure call (except for special forms).
    /// This is an iterative, rather than a recursive one.
    /// </summary>
    public sealed class EvaluateMap : Stepper
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
        /// Initializes a new instance of the EvaluateMap class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="proc">The proc to apply to each element of the list.</param>
        /// <param name="result">The result is appended to this list.</param>
        private EvaluateMap(Stepper parent, object expr, Environment env, Procedure proc, Pair result)
            : base(parent, expr, env)
        {
            this.proc = proc;
            this.result = result;
            this.accum = result;
        }

        /// <summary>
        /// Creates a new map evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="proc">The proc to apply to each element of the list.</param>
        /// <param name="result">The result is appended to this list.</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <returns>The map evaluator.</returns>
        public static EvaluateMap New(object expr, Environment env, Procedure proc, Pair result, Stepper parent)
        {
            return new EvaluateMap(parent, expr, env, proc, result);
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
                            ErrorHandlers.Error("Map: illegal arg list: " + this.Expr);
                            return ReturnFromStep(null);
                        }

                        Pc = PC.Step1;
                        continue;

                    case PC.Step1:
                        if (List.First(this.Expr) is Pair)
                        {
                            // Grab the arguments to the applications (the head of each list).
                            // The the proc is applied to them.
                            object x = this.proc.Apply(this, MapFun(List.First, List.MakeList(Expr)));
                            if (x is Stepper)
                            {
                                return GoToStep((Stepper)x, PC.Step2);
                            }

                            Pc = PC.Step2;
                            ReturnedExpr = x;
                            continue;
                        }

                        // if we are done, just return the result minus the dummy entry
                        return ReturnFromStep(List.Rest(this.result));

                    case PC.Step2:
                        // back from the evaluation -- save the result and keep going with the rest
                        if (this.result != null)
                        {
                            // Builds a list by tacking new values onto the tail.
                            this.accum = (Pair)(this.accum.Rest = List.MakeList(ReturnedExpr));
                        }

                        // Step down each of the lists
                        Expr = MapFun(List.Rest, List.MakeList(Expr));
                        Pc = PC.Step1;
                        continue;
                }

                return ErrorHandlers.EvalError("Map: program counter error");
            }
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
            Pair result = List.MakeList(null);
            Pair accum = result;

            // Iterate down the list, taking the function and building a list of the results.
            expr = List.First(expr);
            // TODO convert to user foreach
            while (expr is Pair)
            {
                // Builds a list by tacking new values onto the tail.
                accum = (Pair)(accum.Rest = List.MakeList(fun(List.First(expr))));
                expr = List.Rest(expr);
            }

            return (Pair)List.Rest(result);
        }
    }
}
