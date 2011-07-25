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
        internal const string StepperName = "evaluate-map";

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
        /// Accumulates the returned result.
        /// The list is constructed in reverse order.
        /// This happens only if returnResult is true;
        /// </summary>
        private Obj result;

        /// <summary>
        /// The lists to map.
        /// </summary>
        private Obj lists;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateMap class.
        /// This is used in the "map" primitive, and as part of "for-each".
        /// Because it is used internally, the evaluator must not use destructive
        ///   operations on its member variables.
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
            this.result = EmptyList.Instance;

            ContinueHere(InitialStep);
            IncrementCounter(counter);
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
        /// <param name="s">The step to evaluate.</param>
        /// <returns>If no expression, return to caller.  Otherwise, continue on to next step.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateMap step = (EvaluateMap)s;

            // first check for degenerate cases
            if (TypePrimitives.IsEmptyList(step.lists))
            {
                return s.ReturnUndefined();
            }

            if (!TypePrimitives.IsPair(step.lists))
            {
                ErrorHandlers.SemanticError("Bad args for map: " + step.lists);
                return s.ReturnUndefined();
            }

            return s.ContinueHere(ApplyFunStep);
        }

        /// <summary>
        /// Apply the map function to an element of the list and grab the result.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>If apply recurses, return that stepper.  Otherwise go on to save result.
        /// If we are done, return the collected results.</returns>
        private static Stepper ApplyFunStep(Stepper s)
        {
            EvaluateMap step = (EvaluateMap)s;
            if (TypePrimitives.IsPair(List.First(step.lists)))
            {
                // Grab the arguments to the applications (the head of each list).
                // Then the proc is applied to them.
                return step.proc.Apply(List.MapFun(List.First, List.New(step.lists)), s.ContinueHere(CollectAndLoopStep));
            }

            // if we are done, return the reversed result list
            return s.ReturnFromStep(step.returnResult ? List.Reverse(step.result) : Undefined.Instance);
        }

        /// <summary>
        /// Collect the result of the function application and loop back to do the next one.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Continue back in apply fun step.</returns>
        private static Stepper CollectAndLoopStep(Stepper s)
        {
            EvaluateMap step = (EvaluateMap)s;

            // back from the evaluation -- save the result and keep going with the rest
            if (step.returnResult)
            {
                // Builds a list by tacking new values onto the head.
                step.result = List.Cons(s.ReturnedExpr, step.result);
            }

            // Step down each of the lists
            step.lists = List.MapFun(List.Rest, List.New(step.lists));
            return s.ContinueHere(ApplyFunStep);
        }
        #endregion
    }
}
