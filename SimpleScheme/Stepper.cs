// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract class Stepper
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
            this.Pc = PC.Initial;
        }

        /// <summary>
        /// The program counter values.
        /// No stepper has more then three steps.
        /// If necessary, more could be added.
        /// </summary>
        public enum PC
        {
            /// <summary>
            /// The program counter starts out here.
            /// </summary>
            Initial,

            /// <summary>
            /// Evaluator Step 1
            /// </summary>
            Step1,

            /// <summary>
            /// Evaluator Step 2
            /// </summary>
            Step2,

            /// <summary>
            /// Evaluator Step 3
            /// </summary>
            Step3,

            /// <summary>
            /// Final step.  On return, the program counter is set to this
            ///   value.  If the step is ever executed again, it will fall through the case
            ///   and trigger an error.
            /// </summary>
            Final
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
        /// Gets the parent that execution returns to when this is done.
        /// </summary>
        public Stepper Parent { get; private set; }

        /// <summary>
        /// Gets or sets the expression being evaluated.  
        /// </summary>
        public object Expr { get; set; }

        /// <summary>
        /// Gets or sets the returned expression from the last call.
        /// </summary>
        public object ReturnedExpr { get; set; }

        /// <summary>
        /// Gets or sets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get; set; }

        /// <summary>
        /// Gets the returned environment from the last call.
        /// </summary>
        public Environment ReturnedEnv { get; private set; }

        /// <summary>
        /// Gets or sets the Evaluators program counter.
        /// Used to sequence through multiple steps.
        /// </summary>
        public PC Pc { get; set; }

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
        /// Dump a stack backtrace
        /// </summary>
        /// <returns></returns>
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
        /// Dump the current step into a string builder.
        /// </summary>
        /// <param name="sb">The string builder to write to.</param>
        private void DumpStep(StringBuilder sb)
        {
            sb.AppendFormat("Step\n  Expr: {0}\n  Pc: {1}\n  Env: {2}", this.Expr, this.Pc, this.Env.Dump(1));
        }

        /// <summary>
        /// Subclasses implement this to make one step.
        /// </summary>
        /// <returns>A step in the evaluation.</returns>
        public abstract Stepper RunStep();

        /// <summary>
        /// Goes to the given step.
        /// Can be used to make a call to a sub-evaluator.
        /// The caller typically creates an evaluator and passes into this method.
        /// The called stepper is responsible for returning the result into ReturnedExpr and ReturnedEnv.
        /// </summary>
        /// <param name="nextStep">The evaluator to go to.</param>
        /// <param name="pc">The next program counter value for this stepper.</param>
        /// <returns>The next step to execute.</returns>
        protected Stepper GoToStep(Stepper nextStep, PC pc)
        {
            this.Pc = pc;
            return nextStep;
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// Store the final PC value to protect against steps after return.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper ReturnFromStep(object expr, Environment env)
        {
            this.Pc = PC.Final;
            return TransferToStep(this.Parent, expr, env);
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// Store the final PC value to protect against steps after return.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper ReturnFromStep(object expr)
        {
            this.Pc = PC.Final;
            return TransferToStep(this.Parent, expr, this.Env);
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but set the expr
        /// </summary>
        /// <param name="expr">The new expr value.</param>
        /// <returns>The next step, which is this stepper.</returns>
        protected Stepper ContinueStep(object expr)
        {
            return TransferToStep(this, expr, this.Env);
        }
    }
}
