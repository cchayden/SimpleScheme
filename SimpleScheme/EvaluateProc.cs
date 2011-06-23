// <copyright file="EvaluateProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    internal class EvaluateProc : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-proc";

        /// <summary>
        /// The proc or primitive to apply.
        /// </summary>
        private readonly Procedure fn;

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateProc class.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        protected EvaluateProc(Procedure fn, Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.fn = fn;
            ContinueHere(this.EvalArgsStep);
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
        /// Call apply proc evaluator.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The apply proc evaluator.</returns>
        internal static Stepper Call(Procedure fn, Obj expr, Stepper caller)
        {
            return new EvaluateProc(fn, expr, caller.Env, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Provide TraceInfo: the name and the proc to execute.
        /// </summary>
        /// <returns>Trace info.</returns>
        internal override string TraceInfo()
        {
            string info = base.TraceInfo();
            return info == null ? null : info + " " + this.fn;
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Back here after args have been evaluated.  
        /// Apply the proc to the evaluated args.  
        /// </summary>
        /// <returns>The result, or the next step to obtain it.</returns>
        protected Stepper ApplyStep()
        {
            return this.fn.Apply(ReturnedExpr, ContinueReturn());
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating all the arguments.
        /// </summary>
        /// <returns>Next action to evaluate the args.</returns>
        private Stepper EvalArgsStep()
        {
            return EvaluateList.Call(Expr, ContinueHere(this.ApplyStep));
        }
        #endregion
    }
}