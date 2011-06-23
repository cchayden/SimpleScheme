// <copyright file="EvaluateProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public class EvaluateProc : Stepper
    {
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

        /// <summary>
        /// Initializes a new instance of the EvaluateProc class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The function to apply.</param>
        protected EvaluateProc(Stepper caller, object expr, Environment env, Procedure fn)
            : base(caller, expr, env)
        {
            this.fn = fn;
            ContinueHere(this.EvalArgsStep);
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
        /// Call apply proc evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="fn">The function to apply.</param>
        /// <returns>The apply proc evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr, Procedure fn)
        {
            return new EvaluateProc(caller, expr, caller.Env, fn);
        }

        /// <summary>
        /// Provide TraceInfo that includes the proc to execute in addition to the name.
        /// </summary>
        /// <returns>Trace info.</returns>
        public override string TraceInfo()
        {
            string name = base.TraceInfo();
            if (name == null)
            {
                return null;
            }

            return name + " " + this.fn;
        }

        /// <summary>
        /// Back here after args have been evaluated.  
        /// Apply the proc to the evaluated args.  
        /// </summary>
        /// <returns>The result, or the next step to obtain it.</returns>
        protected Stepper ApplyStep()
        {
            return this.fn.Apply(ContinueReturn(), ReturnedExpr);
        }

        /// <summary>
        /// Begin by evaluating all the arguments.
        /// </summary>
        /// <returns>Next action to evaluate the args.</returns>
        private Stepper EvalArgsStep()
        {
            return EvaluateList.Call(ContinueHere(this.ApplyStep), Expr);
        }
    }
}