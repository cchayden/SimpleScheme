// <copyright file="Stepper.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract partial class Stepper : SchemeUtils
    {
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
            /// Step 1
            /// </summary>
            Step1,

            /// <summary>
            /// Step 2
            /// </summary>
            Step2,

            /// <summary>
            /// Step 3
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
        /// Create a halt stepper.
        /// This is used to get Eval started.
        /// </summary>
        /// <returns>Adefault stepper.</returns>
        public static Stepper Halt()
        {
            return new EvaluatorInitial();
        }

        /// <summary>
        /// Create a suspend stepper.
        /// This is used when stepper needs to suspend itself.
        /// </summary>
        /// <returns>Adefault stepper.</returns>
        public static Stepper Suspend()
        {
            return new EvaluatorInitial();
        }

        /// <summary>
        /// Evaluate a string by looking it up in the environment.
        /// </summary>
        /// <param name="symbol">The symbol</param>
        /// <param name="env">The environment to evaluate it in.</param>
        /// <returns>The value of the symbol.</returns>
        public static object EvalString(string symbol, Environment env)
        {
            return env.Lookup(symbol);
        }

        /// <summary>
        /// Create a main evaluator.
        /// </summary>
        /// <param name="parent">The caller, to return to after evaluation is over.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to run next.</returns>
        public static Stepper CallEvaluate(Stepper parent, object expr, Environment env)
        {
            return EvaluatorMain.New(parent, expr, env);
        }

        /// <summary>
        /// Create the map evaluator
        /// </summary>
        /// <param name="parent">The caller, to return to after evaluation is over.</param>
        /// <param name="expr">The map list to traverse.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <param name="proc">The map proc.</param>
        /// <param name="result">The result is appended to this.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper CallMap(Stepper parent, object expr, Environment env, Procedure proc, Pair result)
        {
            return new EvaluatorMap(parent, expr, env, proc, result);
        }

        /// <summary>
        /// Create the continuation stepper
        /// </summary>
        /// <param name="parent">The caller, to return to after the continuation proc is applied.</param>
        /// <param name="expr">The (what is this ???).</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper CallContinuation(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorContinuation(parent, expr, env);
        }

        /// <summary>
        /// Create a stepper for call-with-input-file
        /// </summary>
        /// <param name="parent">The caller, to return to after the operation is complete.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper CallWithInputFile(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorCallWithInputFile(parent, expr, env);
        }

        /// <summary>
        /// Create a stepper for call-with-output-file
        /// </summary>
        /// <param name="parent">The caller, to return to after the operation is complete.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper CallWithOutputFile(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorCallWithOutputFile(parent, expr, env);
        }

        /// <summary>
        /// Create a stepper for time-call
        /// </summary>
        /// <param name="parent">The caller, to return to after the operation is complete.</param>
        /// <param name="expr">The expression to evaluate and the number of times to execute it.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper CallTimeCall(Stepper parent, object expr, Environment env)
        {
            return new EvaluatorTimeCall(parent, expr, env);
        }

        /// <summary>
        /// Transfer to a given stepper.  
        /// This can be used to return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// The Call/CC handler uses this to transfer to a saved continuation, otherwise
        ///     is is used only for returns.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <param name="env">The environment to save as the returned environment.</param>
        /// <param name="nextStep">The stepper to transfer to.</param>
        /// <returns>The next step.  This is in the parent for return.</returns>
        public static Stepper TransferToStep(object expr, Environment env, Stepper nextStep)
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
        /// Goes to the given step.
        /// Can be used to make a call to a sub-evaluator.
        /// The caller typically creates an evaluator and passes into this method.
        /// The called stepper is responsible for returning the result into ReturnedExpr and ReturnedEnv.
        /// </summary>
        /// <param name="pc">The next program counter value for this stepper.</param>
        /// <param name="nextStep">The evaluator to go to.</param>
        /// <returns>The next step to execute.</returns>
        protected Stepper GoToStep(PC pc, Stepper nextStep)
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
        protected Stepper SubReturn(object expr, Environment env)
        {
            this.Pc = PC.Final;
            return TransferToStep(expr, env, this.Parent);
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// Store the final PC value to protect against steps after return.
        /// </summary>
        /// <param name="expr">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper SubReturn(object expr)
        {
            this.Pc = PC.Final;
            return TransferToStep(expr, this.Env, this.Parent);
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but set the expr
        /// </summary>
        /// <param name="expr">The new expr value.</param>
        /// <returns>The next step, which is this stepper.</returns>
        protected Stepper SubContinue(object expr)
        {
            return TransferToStep(expr, this.Env, this);
        }

        // Create Sub Evaluators

        /// <summary>
        /// Recursive Stepper call.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <returns>The next evaluation step.</returns>
        protected Stepper CallEval(object args)
        {
            return EvaluatorMain.New(this, args, this.Env);
        }

        /// <summary>
        /// Create a sequence evaluator.
        /// </summary>
        /// <param name="args">The sequence to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallSequence(object args)
        {
            return EvaluatorSequence.New(this, args, this.Env);
        }

        /// <summary>
        /// Create a define evaluator.
        /// </summary>
        /// <param name="args">The define to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallDefine(object args)
        {
            return EvaluatorDefine.New(this, args, this.Env);
        }

        /// <summary>
        /// Create a set! evaluator.
        /// </summary>
        /// <param name="args">The set! to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallSet(object args)
        {
            return EvaluatorSet.New(this, args, this.Env);
        }

        /// <summary>
        /// Create a if evaluator.
        /// </summary>
        /// <param name="args">The if to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallIf(object args)
        {
            return EvaluatorIf.New(this, args, this.Env);
        }

        /// <summary>
        /// Create an or evaluator.
        /// </summary>
        /// <param name="args">The or clauses to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallOr(object args)
        {
            return EvaluatorOr.New(this, args, this.Env);
        }

        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="args">The or clauses to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallAnd(object args)
        {
            return EvaluatorAnd.New(this, args, this.Env);
        }

        /// <summary>
        /// Create a cond evaluator.
        /// </summary>
        /// <param name="args">The cond to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallReduceCond(object args)
        {
            return EvaluatorReduceCond.New(this, args, this.Env);
        }

        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="args">The list to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallList(object args)
        {
            return EvaluatorList.New(this, args, this.Env);
        }

        /// <summary>
        /// Create a apply evaluator.
        /// </summary>
        /// <param name="args">The application to evaluate.</param>
        /// <param name="fn">The proc to apply.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallApplyProc(object args, object fn)
        {
            return EvaluatorApplyProc.New(this, args, this.Env, fn);
        }

        /// <summary>
        /// Create a closure evaluator.
        /// </summary>
        /// <param name="args">The closure to evaluate.</param>
        /// <param name="f">The enclosing closure.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallClosure(object args, Closure f)
        {
            return EvaluatorClosure.New(this, args, this.Env, f);
        }

        /// <summary>
        /// Create a macro expander.
        /// </summary>
        /// <param name="args">The macro args.</param>
        /// <param name="fn">The macro object.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallExpand(object args, Macro fn)
        {
            return EvaluatorExpand.New(this, args, this.Env, fn);
        }
    }
}
