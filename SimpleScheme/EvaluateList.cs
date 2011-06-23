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
        private const string StepperName = "list";

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
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateList(Stepper caller, object expr, Environment env)
            : base(caller, expr, env)
        {
            this.objs = expr;

            // start with an empty list
            // the empty cell will be stripped off at the end
            this.accum = this.result = MakeList(null);
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
            if (expr == null)
            {
                return caller.ContinueStep(null);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.Error("Illegal arg list: " + expr);
                return caller.ContinueStep(null);
            }

            return new EvaluateList(caller, expr, caller.Env);
        }
        
        /// <summary>
        /// Create the list by evaluating the expression.
        /// </summary>
        /// <returns>Next step evaluates the first expression.</returns>
        private Stepper EvalExprStep()
        {
            // there is more to do --  evaluate the first expression
            return EvaluateExpression.Call(ContinueHere(this.LoopStep), First(this.objs));
        }

        /// <summary>
        /// Back from evaluating the expression.  Accumulate the result and, if there
        ///   is anything left, loop back to evaluate another expression.
        /// </summary>
        /// <returns>The created list, or a stepper to loop back and evaluate some more.</returns>
        private Stepper LoopStep()
        {
            // back from the evaluation -- save the result and keep going with the rest
            this.accum = (Pair)(this.accum.RestCell = new Pair(ReturnedExpr, null));
            this.objs = Rest(this.objs);

            if (this.objs is Pair)
            {
                // Come back to this step, so don't assign PC for better performance.
                return EvaluateExpression.Call(this, First(this.objs));
            }

            // We are done now, return
            return ReturnFromStep(this.result.RestCell);
        }
    }
}