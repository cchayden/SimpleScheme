// <copyright file="EvaluateMap.cs" company="Charles Hayden">
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
    //// <r4rs section="6.9">(map proc <list1> <list2> ...)</r4rs>
    internal sealed class EvaluateMap : Stepper
    {
        #region Fields
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
        /// True if the map needs to return a result.
        /// If false, don'b bother to accumulate the result.
        /// Map needs the result, for-each does not.
        /// </summary>
        private readonly bool returnResult;

        /// <summary>
        /// The initial value of the result.
        /// This is either an empty list, or null.
        /// If null, then no result is returned.
        /// </summary>
        private readonly Pair result;

        /// <summary>
        /// Accumulates the returned result.
        /// The is the end of the list that we are constructing (result).
        /// </summary>
        private Pair accum;

        /// <summary>
        /// The lists to map.
        /// </summary>
        private Obj lists;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateMap class.
        /// </summary>
        /// <param name="proc">The proc to apply to each element of the list.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="returnResult">Return the map map result if this is true.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateMap(Procedure proc, Obj expr, Environment env, bool returnResult, Stepper caller)
            : base(expr, env, caller)
        {
            this.proc = proc;
            this.lists = expr;
            this.returnResult = returnResult;
            if (returnResult)
            {
                this.accum = this.result = MakeEmptyList();
            }

            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Call the map evaluator
        /// </summary>
        /// <param name="proc">The map proc.</param>
        /// <param name="expr">The map list to traverse.</param>
        /// <param name="returnResult">If true, return the result of the map.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller -- return to this when done.</param>
        /// <returns>The step to execute.</returns>
        internal static Stepper Call(Procedure proc, Obj expr, bool returnResult, Environment env, Stepper caller)
        {
            return new EvaluateMap(proc, expr, env, returnResult, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start the map evaluation.
        /// Begin with some error checking then go to nex step.
        /// </summary>
        /// <returns>If no expression, return to caller.  Otherwise, continue on to next step.</returns>
        private Stepper InitialStep()
        {
            // first check for degenerate cases
            if (this.lists == List.Empty)
            {
                return ReturnUndefined();
            }

            if (!(this.lists is Pair))
            {
                ErrorHandlers.SemanticError("Bad args for map: " + this.lists);
                return ReturnUndefined();
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
                return this.proc.Apply(MapFun(First, MakeList(this.lists)), this.Env, ContinueHere(this.CollectAndLoopStep));
            }

            // if we are done, just return the result minus the dummy entry
            return ReturnFromStep(this.returnResult ? Rest(this.result) : Undefined.Instance);
        }

        /// <summary>
        /// Collect the result of the function application and loop back to do the next one.
        /// </summary>
        /// <returns>Continue back in apply fun step.</returns>
        private Stepper CollectAndLoopStep()
        {
            // back from the evaluation -- save the result and keep going with the rest
            if (this.returnResult)
            {
                // Builds a list by tacking new values onto the tail.
                this.accum = (Pair)(this.accum.RestCell = MakeList(ReturnedExpr));
            }

            // Step down each of the lists
            this.lists = MapFun(Rest, MakeList(this.lists));
            return ContinueHere(this.ApplyFunStep);
        }
        #endregion
    }
}
