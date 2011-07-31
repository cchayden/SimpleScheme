#define OLD
// <copyright file="EvaluateProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public class EvaluateProc : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-proc";

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
            ContinueHere(EvalArgsStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call apply proc evaluator.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The apply proc evaluator.</returns>
        public static Stepper Call(Procedure fn, Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateProc(fn, expr, env, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Provide TraceInfo: the name and the proc to execute.
        /// </summary>
        /// <returns>Trace info.</returns>
        public override string TraceInfo()
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
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The result, or the next step to obtain it.</returns>
        protected static Stepper ApplyStep(Stepper s)
        {
            EvaluateProc step = (EvaluateProc)s;
            return step.fn.Apply(s.ReturnedExpr, s.Caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Begin by evaluating all the arguments.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Next action to evaluate the args.</returns>
        private static Stepper EvalArgsStep(Stepper s)
        {
            return EvaluateList.Call(s.Expr, s.Env, s.ContinueHere(ApplyStep));
        }
        #endregion
    }
}