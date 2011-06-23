// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    internal abstract class Stepper : ListPrimitives
    {
        #region Fields
        /// <summary>
        /// The suspended stepper is used to indicate suspension, when stepping
        ///   needs to be delayed but is not complete.
        /// </summary>
        internal static readonly Stepper Suspended = new EvaluatorBase("suspended");

        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counterId = Counter.Create(StepperName);

        /// <summary>
        /// Gets the caller that execution returns to when this is done.
        /// </summary>
        private readonly Stepper caller;

        /// <summary>
        /// The program counter.
        /// Contains the function to execute next.
        /// </summary>
        private StepperFunction pc;

        /// <summary>
        /// Indicates whether a trace has been performed on this stepper instance.
        /// </summary>
        private bool traced;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Stepper class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        /// <param name="caller">The caller evaluator.</param>
        protected Stepper(Obj expr, Environment env, Stepper caller)
        {
            this.caller = caller;
            this.Expr = expr;
            this.Env = env;
            this.traced = false;

            this.IncrementCounter(counterId);
        }
        #endregion

        /// <summary>
        /// This is the type for the stepper functions.
        /// These values are assigned to the pc.
        /// </summary>
        /// <returns>The next step to take.</returns>
        internal delegate Stepper StepperFunction();

        #region Accessors
        /// <summary>
        /// Gets the stepper name, used for tracing and counters.
        /// Each subclass must implement.
        /// </summary>
        internal abstract string Name { get; }

        /// <summary>
        /// Gets the expression being evaluated.  
        /// </summary>
        internal Obj Expr { get; private set; }

        /// <summary>
        /// Gets the returned expression from the last call.
        /// </summary>
        internal Obj ReturnedExpr { get; private set; }

        /// <summary>
        /// Gets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        internal Environment Env { get; private set; }

        /// <summary>
        /// Gets the returned environment from the last call.
        /// </summary>
        internal Environment ReturnedEnv { get; private set; }

        /// <summary>
        /// Gets the environment of the caller.
        /// </summary>
        internal Environment CallerEnv
        {
            get { return this.caller.Env; }
        }

        internal Stepper Caller
        {
            get { return this.caller; }
        }

        /// <summary>
        /// Gets the caller's caller.
        /// </summary>
        internal Stepper CallerCaller
        {
            get { return this.caller.caller; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Transfer to a given stepper.  
        /// This can be used to return fram an evaluator.
        /// Assign the return value and return the caller task to resume.
        /// The Call/CC handler uses this to transfer to a saved continuation.
        /// </summary>
        /// <param name="nextStep">The stepper to transfer to.</param>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next step.  This is in the caller for return.</returns>
        internal static Stepper TransferToStep(Stepper nextStep, Obj expr, Environment env)
        {
            nextStep.ReturnedExpr = expr;
            nextStep.ReturnedEnv = env;
            return nextStep;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Trace information for the step.
        /// </summary>
        /// <returns>Info to print for the trace.</returns>
        internal virtual string TraceInfo()
        {
            if (this.traced)
            {
                return null;
            }

            this.traced = true;
            return this.Name;
        }

        /// <summary>
        /// Run the step represented by the PC.
        /// </summary>
        /// <returns>The next step to run.</returns>
        internal Stepper RunStep()
        {
            return this.pc();
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but set the returned expr.
        /// </summary>
        /// <param name="expr">The new expr value.</param>
        /// <returns>The next step, which is this stepper.</returns>
        internal Stepper ContinueStep(Obj expr)
        {
            this.ReturnedExpr = expr;
            return this;
        }

        /// <summary>
        /// Create a stack backtrace
        /// </summary>
        /// <returns>A backtrace of the stepper call stack.</returns>
        internal string StackBacktrace()
        {
            Stepper step = this.caller;    // skip backtrace itself
            StringBuilder sb = new StringBuilder();
            while (step != null)
            {
                step.DumpStep(sb);
                step = step.caller;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Increment the given counter.
        /// </summary>
        /// <param name="counterIdent">The counter id</param>
        internal void IncrementCounter(int counterIdent)
        {
            if (this.Env != null)
            {
                this.Env.Interp.IncrementCounter(counterIdent);
            }
        }

        /// <summary>
        /// Convert the stepper instance to a string.
        /// Could also consider printing the Expr.
        /// </summary>
        /// <param name="quoted">True if the string should be quoted.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        internal void AsString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<stepper>");
            }
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Assign PC and return the current stepper.
        /// </summary>
        /// <param name="nextStep">The new PC value</param>
        /// <returns>The next step to take.</returns>
        protected Stepper ContinueHere(StepperFunction nextStep)
        {
            this.pc = nextStep;
            return this;
        }

        /// <summary>
        /// Assign PC to be the return step;
        /// </summary>
        /// <returns>The next step to take.</returns>
        protected Stepper ContinueReturn()
        {
            this.pc = this.ReturnStep;
            return this;
        }

        /// <summary>
        /// Create a new environment and replace the current one with it.
        /// </summary>
        /// <param name="formals">The environment variable names.</param>
        /// <param name="vals">The values of the variables.</param>
        /// <param name="parent">The caller environment.</param>
        protected void ReplaceEnvironment(Obj formals, Obj vals, Environment parent)
        {
            this.Env = Environment.New(formals, vals, parent);
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the caller task to resume.
        /// Set pc to catch errors.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next step, which is in the caller.</returns>
        protected Stepper ReturnFromStep(Obj expr, Environment env)
        {
            this.caller.ReturnedExpr = expr;
            this.caller.ReturnedEnv = env;
            return this.caller;
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// Set pc to catch errors.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the caller.</returns>
        protected Stepper ReturnFromStep(Obj expr)
        {
            this.caller.ReturnedExpr = expr;
            return this.caller;
        }

        /// <summary>
        /// Return the final result.
        /// This exists as a separate function so that it can be used as a StepperFunction.
        /// </summary>
        /// <returns>The next step, which is in the caller.</returns>
        protected Stepper ReturnStep()
        {
            this.caller.ReturnedExpr = this.ReturnedExpr;
            return this.caller;
        }

        /// <summary>
        /// Return the undefined result.
        /// </summary>
        /// <returns>The next step, which is in the caller.</returns>
        protected Stepper ReturnUndefined()
        {
            this.caller.ReturnedExpr = Undefined.Instance;
            return this.caller;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Dump the current step into a string builder.
        /// </summary>
        /// <param name="sb">The string builder to write to.</param>
        private void DumpStep(StringBuilder sb)
        {
            sb.AppendFormat("Step {0}\n", this.Name);
            string exp = this.Expr == List.Empty ? "()" : this.Expr.ToString();
            sb.AppendFormat("  Expr: {0}\n", exp);
            if (this.Env != null)
            {
                sb.AppendFormat("  Env:\n{0}", this.Env.Dump(1, 3));
            }
        }
        #endregion
    }
}
