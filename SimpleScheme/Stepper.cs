// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract class Stepper : ListPrimitives
    {
        /// <summary>
        /// The suspended stepper is used to indicate suspension, when stepping
        ///   needs to be delayed but is not complete.
        /// </summary>
        private static readonly Stepper suspended = new EvaluatorBase("suspended");

        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counterId = Counter.Create(StepperName);

        /// <summary>
        /// Initializes a new instance of the Stepper class.
        /// </summary>
        /// <param name="caller">The caller evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        protected Stepper(Stepper caller, object expr, Environment env)
        {
            this.Caller = caller;
            this.Expr = expr;
            this.Env = env;

            this.IncrementCounter(counterId);
        }

        /// <summary>
        /// This is the type for the stepper functions.
        /// These values are assigned to the Pc.
        /// </summary>
        /// <returns>The next step to take.</returns>
        public delegate Stepper StepperFunction();

        /// <summary>
        /// Gets the singleton suspended stepper.
        /// This is used when stepper needs to suspend itself.
        /// </summary>
        /// <returns>A default stepper.</returns>
        public static Stepper Suspended
        {
            get { return suspended; }
        }

        /// <summary>
        /// Gets the stepper name, used for tracing and counters.
        /// Each subclass must implement.
        /// </summary>
        public abstract string Name { get; }

        /// <summary>
        /// Gets the next step in the evaluator to execute.
        /// Each evaluator must keep track of its internal program counterId with this delegate.
        /// </summary>
        public StepperFunction Pc { get; private set; }

        /// <summary>
        /// Gets the caller that execution returns to when this is done.
        /// </summary>
        public Stepper Caller { get; private set; }

        /// <summary>
        /// Gets the expression being evaluated.  
        /// </summary>
        public object Expr { get; private set; }

        /// <summary>
        /// Gets the returned expression from the last call.
        /// </summary>
        public object ReturnedExpr { get; private set; }

        /// <summary>
        /// Gets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get; private set; }

        /// <summary>
        /// Gets the returned environment from the last call.
        /// </summary>
        public Environment ReturnedEnv { get; private set; }

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
        public static Stepper TransferToStep(Stepper nextStep, object expr, Environment env)
        {
            nextStep.ReturnedExpr = expr;
            nextStep.ReturnedEnv = env;
            return nextStep;
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but set the returned expr.
        /// </summary>
        /// <param name="expr">The new expr value.</param>
        /// <returns>The next step, which is this stepper.</returns>
        public Stepper ContinueStep(object expr)
        {
            this.ReturnedExpr = expr;
            return this;
        }

        /// <summary>
        /// Create a stack backtrace
        /// </summary>
        /// <returns>A backtrace of the stepper call stack.</returns>
        public string StackBacktrace()
        {
            Stepper step = this.Caller;    // skip backtrace itself
            StringBuilder sb = new StringBuilder();
            while (step != null)
            {
                step.DumpStep(sb);
                step = step.Caller;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Increment the given counter.
        /// </summary>
        /// <param name="counterIdent">The counter id</param>
        public void IncrementCounter(int counterIdent)
        {
            if (this.Env != null)
            {
                this.Env.Interp.IncrementCounter(counterIdent);
            }
        }

        /// <summary>
        /// Assign PC and return the current stepper.
        /// </summary>
        /// <param name="nextStep">The new PC value</param>
        /// <returns>The next step to take.</returns>
        protected Stepper ContinueHere(StepperFunction nextStep)
        {
            this.Pc = nextStep;
            return this;
        }

        /// <summary>
        /// Assign PC to be the return step;
        /// </summary>
        /// <returns>The next step to take.</returns>
        protected Stepper ContinueReturn()
        {
            this.Pc = this.ReturnStep;
            return this;
        }

        /// <summary>
        /// Create a new environment and replace the current one with it.
        /// </summary>
        /// <param name="formals">The environment variable names.</param>
        /// <param name="vals">The values of the variables.</param>
        /// <param name="parent">The caller environment.</param>
        protected void ReplaceEnvironment(object formals, object vals, Environment parent)
        {
            this.Env = new Environment(formals, vals, parent);
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the caller task to resume.
        /// Set Pc to catch errors.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next step, which is in the caller.</returns>
        protected Stepper ReturnFromStep(object expr, Environment env)
        {
            this.Caller.ReturnedExpr = expr;
            this.Caller.ReturnedEnv = env;
            return this.Caller;
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// Set Pc to catch errors.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the caller.</returns>
        protected Stepper ReturnFromStep(object expr)
        {
            this.Caller.ReturnedExpr = expr;
            return this.Caller;
        }

        /// <summary>
        /// Return the final result.
        /// This exists as a separate function so that it can be used as a StepperFunction.
        /// </summary>
        /// <returns>The last expression evaluated.</returns>
        protected Stepper ReturnStep()
        {
            this.Caller.ReturnedExpr = this.ReturnedExpr;
            return this.Caller;
        }

        /// <summary>
        /// Dump the current step into a string builder.
        /// </summary>
        /// <param name="sb">The string builder to write to.</param>
        private void DumpStep(StringBuilder sb)
        {
            sb.AppendFormat("Step {0}\n", this.Name);
            string exp = this.Expr == null ? "()" : this.Expr.ToString();
            sb.AppendFormat("  Expr: {0}\n", exp);
            if (this.Env != null)
            {
                sb.AppendFormat("  Env:\n{0}", this.Env.Dump(1, 3));
            }
        }
    }
}
