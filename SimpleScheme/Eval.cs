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
            this.Expr = expr;
            this.Env = env;
            this.Pc = 0;
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
        /// Gets the returned expression from the last call.
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
        public int Pc { get; set; }


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
        // TODO document
        public static Stepper CallWithInputFile(Scheme interp, Stepper parent, object expr, Environment env)
        {
            return new EvaluatorCallWithInputFile(interp, parent, expr, env);
        }
        public static Stepper CallWithOutputFile(Scheme interp, Stepper parent, object expr, Environment env)
        {
            return new EvaluatorCallWithOutputFile(interp, parent, expr, env);
        }
        public static Stepper CallTimeCall(Scheme interp, Stepper parent, object expr, Environment env)
        {
            return new EvaluatorTimeCall(interp, parent, expr, env);
        }

        /// <summary>
        /// Subclasses implement this to make one step in the evaluation.
        /// </summary>
        /// <returns>A step in the evaluation.</returns>
        public abstract Stepper EvalStep();

        // TODO consider elimination
        /// <summary>
        /// Make a call to a sub-evaluator.
        /// The caller is responsible for returning the result into ReturnedExpr and ReturnedEnv.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Stepper SubCall(Stepper toCall)
        {
            return toCall;
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// </summary>
        /// <param name="retVal">The value to save as the returned value.</param>
        /// <param name="retEnv">The environment to save as the returned environment.</param>
        /// <param name="returnTo">The stepper to return to.</param>
        /// <returns>The next step, which is in the parent.</returns>
        public static Stepper SubTransfer(object retVal, Environment retEnv, Stepper returnTo)
        {
            returnTo.ReturnedExpr = retVal;
            returnTo.ReturnedEnv = retEnv;
            return returnTo;
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// </summary>
        /// <param name="retVal">The value to save as the returned value.</param>
        /// <param name="retEnv">The environment to save as the returned environment.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper SubReturn(object retVal, Environment retEnv)
        {
            return SubTransfer(retVal, retEnv, this.Parent);
        }

        /// <summary>
        /// Return fram an evaluator, with the default environment
        /// </summary>
        /// <param name="retVal">The value to save as the returned value.</param>
        /// <returns>The next step, which is in the parent.</returns>
        protected Stepper SubReturn(object retVal)
        {
            return SubTransfer(retVal, this.Env, this.Parent);
        }

        /// <summary>
        /// Continue executing in the existing evaluator.
        /// </summary>
        /// <returns>The next step, which is in the same stepper.</returns>
        protected Stepper SubContinue()
        {
            return this.SubCall(this);
        }

        /// <summary>
        /// Continue executing in the existing evaluator, but set the expr
        /// </summary>
        /// <param name="retExpr">The new expr value.</param>
        /// <returns>The next step, which is this stepper.</returns>
        protected Stepper SubContinue(object retExpr)
        {
            return SubTransfer(retExpr, Env, this);
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
        /// Create an or evaluator.
        /// </summary>
        /// <param name="args">The or clauses to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallOr(object args)
        {
            return this.SubCall(new EvaluatorOr(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create an and evaluator.
        /// </summary>
        /// <param name="args">The or clauses to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Stepper CallAnd(object args)
        {
            return this.SubCall(new EvaluatorAnd(this.Interp, this, args, this.Env));
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
