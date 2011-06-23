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
            this.Pc = this.InitialStep;
            IncrementCounter("map");
        }

        /// <summary>
        /// Call the map evaluator
        /// </summary>
        /// <param name="caller">The caller -- return to this when done.</param>
        /// <param name="expr">The map list to traverse.</param>
        /// <param name="proc">The map proc.</param>
        /// <param name="result">The result is appended to this.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper Call(Stepper caller, object expr, Procedure proc, Pair result)
        {
            return new EvaluateMap(caller, expr, caller.Env, proc, result);
        }

        /// <summary>
        /// Start the map evaluation.
        /// Begin with some error checking then go to nex step.
        /// </summary>
        /// <returns>If no expression, return to caller.  Otherwise, continue on to next step.</returns>
        private Stepper InitialStep()
        {
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

            this.Pc = this.ApplyFunStep;
            return this;
        }

        /// <summary>
        /// Apply the map function to an element of the list and grab the result.
        /// </summary>
        /// <returns>If apply recurses, return that stepper.  Otherwise go on to save result.
        /// If we are done, return the collected results.</returns>
        private Stepper ApplyFunStep()
        {
            if (List.First(this.Expr) is Pair)
            {
                // Grab the arguments to the applications (the head of each list).
                // Then the proc is applied to them.
                this.Pc = this.CollectAndLoopStep;
                return this.proc.Apply(this, List.MapFun(List.First, List.MakeList(Expr)));
            }

            // if we are done, just return the result minus the dummy entry
            return ReturnFromStep(List.Rest(this.result));
        }

        /// <summary>
        /// Collect the result of the function application and loop back to do the next one.
        /// </summary>
        /// <returns>Continue back in step 1.</returns>
        private Stepper CollectAndLoopStep()
        {
            // back from the evaluation -- save the result and keep going with the rest
            if (this.result != null)
            {
                // Builds a list by tacking new values onto the tail.
                this.accum = (Pair)(this.accum.Rest = List.MakeList(ReturnedExpr));
            }

            // Step down each of the lists
            this.Pc = this.ApplyFunStep;
            return this.LoopStep(List.MapFun(List.Rest, List.MakeList(Expr)));
        }
    }
}
