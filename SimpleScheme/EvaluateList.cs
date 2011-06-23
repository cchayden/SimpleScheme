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
    public sealed class EvaluateList : Stepper
    {
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-list";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The result that will be returned.
        /// </summary>
        private readonly Pair result;

        /// <summary>
        /// Objects to make into a list.
        /// </summary>
        private object objs;

        /// <summary>
        /// The end of the list we are constructing for return.
        /// </summary>
        private Pair accum;

        /// <summary>
        /// Initializes a new instance of the EvaluateList class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateList(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
            this.objs = expr;

            // Start with an empty list.
            // The empty cell will be stripped off at the end.
            this.accum = this.result = MakeEmptyList();
            ContinueHere(this.EvalExprStep);
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
        /// Create a list evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>A list evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            // first check for degenerate cases
            if (expr == List.Empty)
            {
                return caller.ContinueStep(List.Empty);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.Error("Illegal arg list: " + expr);
                return caller.ContinueStep(Undefined.Instance);
            }

            return new EvaluateList(expr, caller.Env, caller);
        }
        
        /// <summary>
        /// Create the list by evaluating the expression.
        /// </summary>
        /// <returns>Next step evaluates the first expression.</returns>
        private Stepper EvalExprStep()
        {
            // there is more to do --  evaluate the first expression
            return EvaluateExpression.Call(First(this.objs), ContinueHere(this.LoopStep));
        }

        /// <summary>
        /// Back from evaluating the expression.  Accumulate the result and, if there
        ///   is anything left, loop back to evaluate another expression.
        /// </summary>
        /// <returns>The created list, or a stepper to loop back and evaluate some more.</returns>
        private Stepper LoopStep()
        {
            // back from the evaluation -- save the result and keep going with the rest
            this.accum = (Pair)(this.accum.RestCell = MakeList(ReturnedExpr));
            this.objs = Rest(this.objs);

            if (this.objs is Pair)
            {
                // Come back to this step, so don't assign PC for better performance.
                return EvaluateExpression.Call(First(this.objs), this);
            }

            // We are done now, return
            return ReturnFromStep(this.result.RestCell);
        }
    }
}