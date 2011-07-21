// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract class Stepper
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

        #region Delegates
        /// <summary>
        /// This is the type for the stepper functions.
        /// It takes no arguments and returns a Stepper.
        /// These values are assigned to the pc.
        /// </summary>
        /// <returns>The next step to take.</returns>
        public delegate Stepper StepperFunction();
        #endregion

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
        /// Gets the caller of this stepper.
        /// </summary>
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

        /// <summary>
        /// Gets the current input port.
        /// </summary>
        internal InputPort CurrentInputPort
        {
            get { return this.Env.Interp.Input;  }
        }

        /// <summary>
        /// Gets the current output port.
        /// </summary>
        internal OutputPort CurrentOutputPort
        {
            get { return this.Env.Interp.Output;  }
        }

        /// <summary>
        /// Gets the current counters class.
        /// </summary>
        internal Counter CurrentCounters
        {
            get { return this.Env.Interp.Counters; }
        }

        /// <summary>
        /// Gets or sets a value indicating whether to trace.
        /// </summary>
        internal bool TraceFlag
        {
            get { return this.Env.Interp.Trace; }
            set { this.Env.Interp.Trace = value; }
        }

        /// <summary>
        /// Gets or sets a value indicating whether to count instructions.
        /// </summary>
        internal bool CountFlag
        {
            get { return this.Env.Interp.Count; }
            set { this.Env.Interp.Count = value; }
        }
        #endregion

        #region Internal Static Methods
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

        #region Internal Methods
        /// <summary>
        /// Trace information for the step.
        /// Do this only once per instance.
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
        /// Just execute the stepper function stored in pc.
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
        /// Call the interpreter in the environment to start evaluating steps.
        /// </summary>
        /// <returns>The return value of the evaluation (or halted or suspended).</returns>
        internal Obj EvalStep()
        {
            return this.Env.Interp.EvalStep(this);
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
        /// Load the given file.
        /// </summary>
        /// <param name="filename">The file to load.</param>
        internal void LoadFile(Obj filename)
        {
            this.Env.Interp.LoadFile(filename);
        }

        /// <summary>
        /// Turn the transcript on.
        /// </summary>
        /// <param name="filename">Write the transcript to this file.</param>
        internal void TranscriptOn(Obj filename)
        {
            this.Env.Interp.TranscriptOn(filename);
        }

        /// <summary>
        /// Turn the transcript off.
        /// </summary>
        internal void TranscriptOff()
        {
            this.Env.Interp.TranscriptOff();
        }

        /// <summary>
        /// Log input to the transcript file.
        /// </summary>
        /// <param name="str">The input to log.</param>
        /// <param name="port">The port that it came from.</param>
        internal void LogInput(string str, InputPort port)
        {
            this.Env.Interp.LogInput(str, port);
        }

        /// <summary>
        /// Log output to the transcript file.
        /// </summary>
        /// <param name="str">The output to log.</param>
        /// <param name="port">The port that it was written to.</param>
        internal void LogOutput(string str, OutputPort port)
        {
            this.Env.Interp.LogOutput(str, port);
        }
        #endregion

        #region Protected Methods
        /// <summary>
        /// Create a new environment and replace the current one with it.
        /// </summary>
        /// <param name="formals">The environment variable names.</param>
        /// <param name="vals">The values of the variables.</param>
        /// <param name="parent">The existing environment.</param>
        protected void ReplaceEnvironment(Obj formals, Obj vals, Environment parent)
        {
            this.Env = new Environment(formals, vals, parent.LexicalParent);
        }

        /// <summary>
        /// Push a new environment made up of the formals and their values.
        /// Link it to the parent environment.
        /// </summary>
        /// <param name="formals">The environment variable names.</param>
        /// <param name="vals">The values of the variables.</param>
        /// <param name="parent">The lexically enclosing environment.</param>
        protected void PushEnvironment(Obj formals, Obj vals, Environment parent)
        {
            this.Env = new Environment(formals, vals, parent);
        }

        /// <summary>
        /// Push an empty environment.
        /// </summary>
        /// <param name="parent">The lexically enclosing environment.</param>
        protected void PushEmptyEnvironment(Environment parent)
        {
            this.Env = new Environment(parent);
        }

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
        /// Return fram an evaluator.
        /// Assign the return value and return the caller task to resume.
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
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the caller.</returns>
        protected Stepper ReturnFromStep(Obj expr)
        {
            this.caller.ReturnedExpr = expr;
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
        /// <param name="buf">The string builder to write to.</param>
        private void DumpStep(StringBuilder buf)
        {
            buf.AppendFormat("Step {0}\n", this.Name);
            string exp = TypePrimitives.IsEmptyList(this.Expr) ? "()" : this.Expr.ToString();
            buf.AppendFormat("  Expr: {0}\n", exp);
            if (this.Env != null)
            {
                buf.AppendFormat("  Env:\n{0}", this.Env.Dump(1, 3));
            }
        }
        #endregion
    }
}
