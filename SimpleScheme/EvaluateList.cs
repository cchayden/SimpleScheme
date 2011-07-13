// <copyright file="EvaluateList.cs" company="Charles Hayden">
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
    //// <r4rs section="6.3">(list <obj> ...)</r4rs>
    internal sealed class EvaluateList : Stepper
    {
        #region Fields
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
        /// Objs to make into a list.
        /// </summary>
        private Obj objs;

        /// <summary>
        /// The end of the list we are constructing for return.
        /// </summary>
        private Pair accum;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateList class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateList(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.objs = expr;

            // Start with an empty list.
            // The empty cell will be stripped off at the end.
            this.accum = this.result = MakeEmptyList();
            ContinueHere(this.EvalExprStep);
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
        /// Create a list evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>A list evaluator.</returns>
        internal static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            // first check for degenerate cases
            if (EmptyList.IsType(expr))
            {
                return caller.ContinueStep(EmptyList.Instance);
            }

            if (!(expr is Pair))
            {
                ErrorHandlers.SemanticError("Bad args for list: " + expr);
                return caller.ContinueStep(Undefined.Instance);
            }

            return new EvaluateList(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Create the list by evaluating the expression.
        /// </summary>
        /// <returns>Next step evaluates the first expression.</returns>
        private Stepper EvalExprStep()
        {
            // there is more to do --  evaluate the first expression
            return EvaluateExpression.Call(First(this.objs), this.Env, ContinueHere(this.LoopStep));
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
                return EvaluateExpression.Call(First(this.objs), this.Env, this);
            }

            // We are done now, return
            return ReturnFromStep(this.result.RestCell);
        }
        #endregion
    }
}