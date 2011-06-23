// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
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
        /// <param name="interp">The scheme interpreter.</param>
        /// <param name="parent">The parent evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        protected Stepper(Scheme interp, Stepper parent, object expr, Environment env)
        {
            this.Interp = interp;
            this.Parent = parent;
            this.RetExpr = this.Expr = expr;
            this.RetEnv = this.Env = env;
            this.Pc = 0;
            this.Called = null;
        }

        /// <summary>
        /// Gets the scheme interpreter.
        /// </summary>
        public Scheme Interp { get; private set; }

        /// <summary>
        /// Gets the parent that execution returns to when this is done.
        /// </summary>
        public Stepper Parent { get; private set; }

        /// <summary>
        /// Gets or sets the expression being evaluated.  
        /// </summary>
        public object Expr { get; set; }

        /// <summary>
        /// Gets or sets the returned expression.
        /// This is valid after a call has completed, and holds the
        ///   returned result.
        /// </summary>
        public object RetExpr { get; set; }

        /// <summary>
        /// Gets or sets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get; set; }

        /// <summary>
        /// Gets or sets the returned environment
        /// </summary>
        public Environment RetEnv { private get; set; }

        /// <summary>
        /// Gets or sets the Evaluators program counter.
        /// Used to sequence through multiple steps.
        /// </summary>
        public int Pc { get; set; }

        /// <summary>
        /// Gets or sets the called sub-evaluator.
        /// The evaluator that is called is stored, so that the returned value
        ///   can be extracted after it returns.
        /// </summary>
        public Stepper Called { private get; set; }

        /// <summary>
        /// Gets the returned expression from the last call.
        /// </summary>
        public object ReturnedExpr
        {
            get { return this.Called.RetExpr; }
        }

        /// <summary>
        /// Gets the returned environment from the last call.
        /// </summary>
        public Environment ReturnedEnv
        {
            get { return this.Called.RetEnv; }
        }

        // Standalone evaluators

        /// <summary>
        /// Create a main evaluator.
        /// </summary>
        /// <param name="interp">The interpreter to use diring evaluation.</param>
        /// <param name="parent">The caller, to return to after evaluation is over.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to run next.</returns>
        public static Stepper CallMain(Scheme interp, Stepper parent, object expr, Environment env)
        {
            return new EvaluatorMain(interp, parent, expr, env);
        }

        /// <summary>
        /// Create the map evaluator
        /// </summary>
        /// <param name="interp">The interpreter to use during evaluation.</param>
        /// <param name="parent">The caller, to return to after evaluation is over.</param>
        /// <param name="expr">The map list to traverse.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <param name="proc">The map proc.</param>
        /// <param name="result">The result is appended to this.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper CallMap(Scheme interp, Stepper parent, object expr, Environment env, Procedure proc, Pair result)
        {
            return new EvaluatorMap(interp, parent, expr, env, proc, result);
        }

        /// <summary>
        /// Create the continuation evaluator
        /// </summary>
        /// <param name="interp">The interpreter to use.</param>
        /// <param name="parent">The caller, to return to after the continuation proc is applied.</param>
        /// <param name="expr">The (what is this ???).</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to execute.</returns>
        public static Stepper CallContinuation(Scheme interp, Stepper parent, object expr, Environment env)
        {
            return new EvaluatorContinuation(interp, parent, expr, env);
        }

        /// <summary>
        /// Subclasses implement this to make one step in the evaluation.
        /// </summary>
        /// <returns>A step in the evaluation.</returns>
        public abstract Stepper EvalStep();

        /// <summary>
        /// Make a call to a sub-evaluator.
        /// When that returns, store the result but do not return.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Stepper SubCall(Stepper toCall)
        {
            this.Called = toCall;
            return toCall;
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// </summary>
        /// <param name="retVal">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper SubReturn(object retVal)
        {
            this.RetExpr = retVal;
            return this.Parent;
        }

        public Stepper SubReturn(object retVal, Stepper returnTo)
        {
            this.RetExpr = retVal;
            return returnTo;
        }

        /// <summary>
        /// Continue executing in the existing evaluator.
        /// </summary>
        /// <returns>The next step, which is in the same evaluator.</returns>
        protected Stepper SubContinue()
        {
            return this.SubCall(this);
        }

        // Sub Evaluators

        /// <summary>
        /// Recursive Stepper call.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <returns>The next evaluation step.</returns>
        protected Stepper CallEval(object args)
        {
            return this.SubCall(new EvaluatorMain(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Recursive Stepper call in the global environment.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <returns>The next evaluation step.</returns>
        protected Stepper CallEvalGlobal(object args)
        {
            return this.SubCall(new EvaluatorMain(this.Interp, this, args, this.Interp.GlobalEnvironment));
        }

        /// <summary>
        /// Create a sequence evaluator.
        /// </summary>
        /// <param name="args">The sequence to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallSequence(object args)
        {
            return this.SubCall(new EvaluatorSequence(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a define evaluator.
        /// </summary>
        /// <param name="args">The define to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallDefine(object args)
        {
            return this.SubCall(new EvaluatorDefine(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a set! evaluator.
        /// </summary>
        /// <param name="args">The set! to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallSet(object args)
        {
            return this.SubCall(new EvaluatorSet(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a if evaluator.
        /// </summary>
        /// <param name="args">The if to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallIf(object args)
        {
            return this.SubCall(new EvaluatorIf(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a cond evaluator.
        /// </summary>
        /// <param name="args">The cond to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallReduceCond(object args)
        {
            return this.SubCall(new EvaluatorReduceCond(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="args">The list to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallList(object args)
        {
            return this.SubCall(new EvaluatorList(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a apply evaluator.
        /// </summary>
        /// <param name="args">The application to evaluate.</param>
        /// <param name="fn">The proc to apply.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallApplyProc(object args, object fn)
        {
            return this.SubCall(new EvaluatorApplyProc(this.Interp, this, args, this.Env, fn));
        }

        /// <summary>
        /// Create a closure evaluator.
        /// </summary>
        /// <param name="args">The closure to evaluate.</param>
        /// <param name="f">The enclosing closure.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallClosure(object args, Closure f)
        {
            return this.SubCall(new EvaluatorClosure(this.Interp, this, args, this.Env, f));
        }

        /// <summary>
        /// Create a macro expander.
        /// </summary>
        /// <param name="args">The macro args.</param>
        /// <param name="fn">The macro object.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallExpand(object args, Macro fn)
        {
            return this.SubCall(new EvaluatorExpand(this.Interp, this, args, this.Env, fn));
        }
    }
}
