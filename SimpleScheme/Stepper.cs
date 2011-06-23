// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public class Stepper
    {
        /// <summary>
        /// The suspended stepper is used to indicate suspension, when stepping
        ///   needs to be delayed but is not complete.
        /// </summary>
        private static readonly Stepper suspended = new EvaluatorBase("suspended");

        /// <summary>
        /// Initializes a new instance of the Stepper class.
        /// </summary>
        /// <param name="parent">The parent evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        protected Stepper(Stepper parent, object expr, Environment env)
        {
            this.Parent = parent;
            this.Expr = expr;
            this.Env = env;
            this.Pc = DefaultStep;
            this.IncrementCounter("evaluate");
        }

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
        /// Gets or sets the next step in the evaluator to execute.
        /// Each evaluator must keep track of its internal program counter with this delegate.
        /// </summary>
        public Func<Stepper> Pc { get; set; }

        /// <summary>
        /// Gets the parent that execution returns to when this is done.
        /// </summary>
        public Stepper Parent { get; private set; }

        /// <summary>
        /// Gets the expression being evaluated.  
        /// </summary>
        public object Expr { get; private set; }

        /// <summary>
        /// Gets the returned expression from the last call.
        /// </summary>
        public object ReturnedExpr { get; private set; }

        /// <summary>
        /// Gets or sets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get; set; }

        /// <summary>
        /// Gets the returned environment from the last call.
        /// </summary>
        public Environment ReturnedEnv { get; private set; }

        /// <summary>
        /// Transfer to a given stepper.  
        /// This can be used to return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// The Call/CC handler uses this to transfer to a saved continuation, otherwise
        ///     is is used only for returns.
        /// </summary>
        /// <param name="nextStep">The stepper to transfer to.</param>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next step.  This is in the parent for return.</returns>
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
        /// Loop: copy result expr and env into working expr and env and
        ///   continue executing in the current stepper.
        /// </summary>
        /// <param name="expr">The expression to assign to the working expr.</param>
        /// <param name="env">The environment to assign to working environment.</param>
        /// <returns>The current stepper.</returns>
        public Stepper LoopStep(object expr, Environment env)
        {
            this.Expr = expr;
            this.Env = env;
            return this;
        }

        /// <summary>
        /// Loop: copy result expr to working expr and continue in the current stepper.
        /// </summary>
        /// <param name="expr">The expression to assign to the working expr.</param>
        /// <returns>The current stepper.</returns>
        public Stepper LoopStep(object expr)
        {
            this.Expr = expr;
            return this;
        }

        /// <summary>
        /// Dump a stack backtrace
        /// </summary>
        /// <returns>A dump of the stepper call stack.</returns>
        public string Dump()
        {
            Stepper step = this;
            StringBuilder sb = new StringBuilder();
            while (step != null)
            {
                step.DumpStep(sb);
                step = step.Parent;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Run one step in a stepper.
        /// Each stepper is responsible for remembering where it left off, which is what it 
        ///   does by setting Pc.
        /// </summary>
        /// <returns>A step in the evaluation.</returns>
        public Stepper RunStep()
        {
            return this.Pc();
        }

        /// <summary>
        /// Increment the named counter.
        /// </summary>
        /// <param name="name">The counter name</param>
        public void IncrementCounter(string name)
        {
#if COUNTERS
            if (this.Env == null)
            {
                return;
            }

            this.Env.Interp.Counters.Increment(name);
#endif
        }

        /// <summary>
        /// Return the final result.
        /// </summary>
        /// <returns>The last expression evaluated.</returns>
        protected Stepper ReturnStep()
        {
            return this.ReturnFromStep(this.ReturnedExpr);
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// Set Pc to catch errors.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper ReturnFromStep(object expr, Environment env)
        {
            this.Pc = DefaultStep;
            return TransferToStep(this.Parent, expr, env);
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// Set Pc to catch errors.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper ReturnFromStep(object expr)
        {
            this.Pc = DefaultStep;
            return TransferToStep(this.Parent, expr, this.Env);
        }

        /// <summary>
        /// Used to indicate a bad step.
        /// Pc initialized to this, and set to this after return.
        /// </summary>
        /// <returns>Does not return.</returns>
        private static Stepper DefaultStep()
        {
            return ErrorHandlers.EvalError("Bad program counter");
        }

        /// <summary>
        /// Dump the current step into a string builder.
        /// </summary>
        /// <param name="sb">The string builder to write to.</param>
        private void DumpStep(StringBuilder sb)
        {
            sb.AppendFormat("Step\n  Expr: {0}\n  Pc: {1}\n  Env: {2}", this.Expr, this.Pc, this.Env.Dump(1));
        }
    }
}
