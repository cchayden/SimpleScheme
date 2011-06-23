// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract class Stepper : SchemeUtils
    {
        /// <summary>
        /// The suspend stepper is used to indicate suspension, when stepping
        ///   needs to be delayed but is not complete.
        /// </summary>
        private static readonly Stepper suspend = new EvaluatorBase();

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
        /// Gets the singleton suspend stepper.
        /// This is used when stepper needs to suspend itself.
        /// </summary>
        /// <returns>A default stepper.</returns>
        public static Stepper Suspend
        {
            get { return suspend; }
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

        // Standalone evaluators

        /// <summary>
        /// Create a main evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <param name="parent">The caller, to return to after evaluation is over.</param>
        /// <returns>The step to run next.</returns>
        public static Stepper CallEvaluate(object expr, Environment env, Stepper parent)
        {
            return EvaluatorMain.New(expr, env, parent);
        }

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
        /// Subclasses implement this to make one step.
        /// </summary>
        /// <returns>A step in the evaluation.</returns>
        public abstract Stepper RunStep();

        /// <summary>
        /// Create the map evaluator
        /// </summary>
        /// <param name="expr">The map list to traverse.</param>
        /// <param name="proc">The map proc.</param>
        /// <param name="result">The result is appended to this.</param>
        /// <returns>The step to execute.</returns>
        public Stepper CallMap(object expr, Procedure proc, Pair result)
        {
            return EvaluateMap.New(expr, this.Env, proc, result, this);
        }

        /// <summary>
        /// Create the continuation stepper
        /// </summary>
        /// <param name="expr">The (what is this ???).</param>
        /// <returns>The step to execute.</returns>
        public Stepper CallContinuation(object expr)
        {
            return EvaluateContinuation.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a stepper for call-with-input-file
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The step to execute.</returns>
        public Stepper CallWithInputFile(object expr)
        {
            return EvaluateCallWithInputFile.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a stepper for call-with-output-file
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The step to execute.</returns>
        public Stepper CallWithOutputFile(object expr)
        {
            return EvaluateCallWithOutputFile.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a stepper for time-call
        /// </summary>
        /// <param name="expr">The expression to evaluate and the number of times to execute it.</param>
        /// <returns>The step to execute.</returns>
        public Stepper CallTimeCall(object expr)
        {
            return EvaluateTimeCall.New(expr, this.Env, this);
        }

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

        // Create Sub Evaluators

        /// <summary>
        /// Recursive Stepper call.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The next evaluation step.</returns>
        protected Stepper CallEval(object expr)
        {
            return EvaluatorMain.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a sequence evaluator.
        /// </summary>
        /// <param name="expr">The sequence to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallSequence(object expr)
        {
            return EvaluateSequence.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a define evaluator.
        /// </summary>
        /// <param name="expr">The define to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallDefine(object expr)
        {
            return EvaluateDefine.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a set! evaluator.
        /// </summary>
        /// <param name="expr">The set! to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallSet(object expr)
        {
            return EvaluateSet.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a if evaluator.
        /// </summary>
        /// <param name="expr">The if to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallIf(object expr)
        {
            return EvaluateIf.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create an or evaluator.
        /// </summary>
        /// <param name="expr">The or clauses to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallOr(object expr)
        {
            return EvaluateOr.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="expr">The or clauses to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallAnd(object expr)
        {
            return EvaluateAnd.New(expr, this.Env, this);
        }

        /// <summary>
        /// Create a cond evaluator.
        /// </summary>
        /// <param name="expr">The cond to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallReduceCond(object expr)
        {
            return EvaluateCond.New(this, expr, this.Env);
        }

        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="args">The list to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallList(object args)
        {
            return EvaluateList.New(args, this.Env, this);
        }

        /// <summary>
        /// Create a apply evaluator.
        /// </summary>
        /// <param name="expr">The application to evaluate.</param>
        /// <param name="fn">The proc to apply.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallApplyProc(object expr, object fn)
        {
            return EvaluateApplyProc.New(expr, this.Env, fn, this);
        }

        /// <summary>
        /// Create a closure evaluator.
        /// </summary>
        /// <param name="expr">The closure to evaluate.</param>
        /// <param name="f">The enclosing closure.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallClosure(object expr, Closure f)
        {
            return EvaluateClosure.New(expr, this.Env, f, this);
        }

        /// <summary>
        /// Create a macro expander.
        /// </summary>
        /// <param name="expr">The macro args.</param>
        /// <param name="fn">The macro object.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallExpandMacro(object expr, Macro fn)
        {
            return EvaluateExpandMacro.New(expr, this.Env, fn, this);
        }
    }
}
