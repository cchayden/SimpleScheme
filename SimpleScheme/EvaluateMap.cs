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
    public sealed class EvaluateMap : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-map";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

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
        /// The lists to map
        /// </summary>
        private object lists;

        /// <summary>
        /// Accumulates the returned result.
        /// The is the end of the list that we are constructing.
        /// </summary>
        private Pair accum;

        /// <summary>
        /// Initializes a new instance of the EvaluateMap class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="proc">The proc to apply to each element of the list.</param>
        /// <param name="result">The result is appended to this list.</param>
        private EvaluateMap(Stepper caller, object expr, Environment env, Procedure proc, Pair result)
            : base(caller, expr, env)
        {
            this.proc = proc;
            this.lists = expr;
            this.result = result;
            this.accum = result;
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
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
            if (this.lists == null)
            {
                return ReturnFromStep(null);
            }

            if (!(this.lists is Pair))
            {
                ErrorHandlers.Error("Map: illegal arg list: " + this.lists);
                return ReturnFromStep(null);
            }

            return ContinueHere(this.ApplyFunStep);
        }

        /// <summary>
        /// Apply the map function to an element of the list and grab the result.
        /// </summary>
        /// <returns>If apply recurses, return that stepper.  Otherwise go on to save result.
        /// If we are done, return the collected results.</returns>
        private Stepper ApplyFunStep()
        {
            if (First(this.lists) is Pair)
            {
                // Grab the arguments to the applications (the head of each list).
                // Then the proc is applied to them.
                return this.proc.Apply(ContinueHere(this.CollectAndLoopStep), MapFun(First, MakeList(this.lists)));
            }

            // if we are done, just return the result minus the dummy entry
            return ReturnFromStep(Rest(this.result));
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
                this.accum = (Pair)(this.accum.RestCell = MakeList(ReturnedExpr));
            }

            // Step down each of the lists
            this.lists = MapFun(Rest, MakeList(this.lists));
            return ContinueHere(this.ApplyFunStep);
        }
    }
}
