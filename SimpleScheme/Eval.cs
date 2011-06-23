// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract partial class Evaluator : SchemeUtils
    {
        /// <summary>
        /// This holds the "state" of the evaluator, what would normally go in the
        ///   execution stack.
        /// </summary>
        private readonly ActivationRecord frame;

        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// </summary>
        /// <param name="interp">The scheme interpreter.</param>
        /// <param name="parent">The parent evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        protected Evaluator(Scheme interp, Evaluator parent, object expr, Environment env)
        {
            frame = new ActivationRecord(interp, parent, expr, env);
        }

        /// <summary>
        /// Gets the frame.
        /// </summary>
        private ActivationRecord Frame { get { return frame; } }

        /// <summary>
        /// Gets the interpreter.
        /// </summary>
        protected Scheme Interp
        {
            get { return frame.Interp; }
        }

        /// <summary>
        /// Gets the parent (the caller).
        /// </summary>
        protected Evaluator Parent
        {
            get { return frame.Parent; }
        }

        /// <summary>
        /// Gets or sets the expression being evaluated.  
        /// </summary>
        public object Expr
        {
            get { return frame.Expr; }
            set { frame.Expr = value; }
        }

        /// <summary>
        /// Gets or sets the returned expression.
        /// This is valid after a call has completed, and holds the
        ///   returned result.
        /// </summary>
        public object RetExpr
        {
            get { return frame.RetExpr; }
            set { frame.RetExpr = value; }
        }

        /// <summary>
        /// Gets or sets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env
        {
            get { return frame.Env; }
            set { frame.Env = value; }
        }

        /// <summary>
        /// Gets or sets the returned environment
        /// </summary>
        public Environment RetEnv
        {
            set { frame.RetEnv = value; }
        }

        /// <summary>
        /// Gets or sets the Evaluators program counter.
        /// Used to sequence through multiple steps.
        /// </summary>
        protected int Pc
        {
            get { return frame.Pc; }
            set { frame.Pc = value; }
        }

        /// <summary>
        /// Gets the expression returned by the last call.
        /// </summary>
        protected object ReturnedExpr
        {
            get { return frame.CalledRetExpr; }
        }

        /// <summary>
        /// Gets the environment returned by the last call.
        /// </summary>
        protected Environment ReturnedEnv
        {
            get { return frame.CalledRetEnv; }
        }

        /// <summary>
        /// Subclasses implement this to make one step in the evaluation.
        /// </summary>
        /// <returns></returns>
        public abstract Evaluator EvalStep();

        /// <summary>
        /// Make a call to a sub-evaluator.
        /// When that returns, store the result but do not return.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Evaluator SubCall(Evaluator toCall)
        {
            frame.Called = toCall;
            return toCall;
        }

        /// <summary>
        /// Return fram an evaluator.
        /// Assign the return value and return the parent task to resume.
        /// </summary>
        /// <param name="retVal"></param>
        /// <returns></returns>
        protected Evaluator SubReturn(object retVal)
        {
            frame.RetExpr = retVal;
            return frame.Parent;
        }

        /// <summary>
        /// Continue executing in the existing evaluator.
        /// </summary>
        /// <returns></returns>
        protected Evaluator SubContinue()
        {
            frame.Called = null;
            return this;
        }

        /// <summary>
        /// Create a main evaluator.
        /// </summary>
        /// <param name="interp">The interpreter to use diring evaluation.</param>
        /// <param name="parent">The caller, to return to after evaluation is over.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment.</param>
        /// <returns>The step to run next.</returns>
        public static Evaluator CallMain(Scheme interp, Evaluator parent, object expr, Environment env)
        {
            return new EvaluatorMain(interp, parent, expr, env);
        }

        /// <summary>
        /// Recursive Evaluator call.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <returns>The next evaluation step.</returns>
        protected Evaluator CallEval(object args)
        {
            return SubCall(new EvaluatorMain(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a sequence evaluator.
        /// </summary>
        /// <param name="args">The sequence to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Evaluator CallSequence(object args)
        {
            return SubCall(new EvaluatorSequence(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a define evaluator.
        /// </summary>
        /// <param name="args">The define to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Evaluator CallDefine(object args)
        {
            return SubCall(new EvaluatorDefine(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a set! evaluator.
        /// </summary>
        /// <param name="args">The set! to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Evaluator CallSet(object args)
        {
            return SubCall(new EvaluatorSet(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a if evaluator.
        /// </summary>
        /// <param name="args">The if to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Evaluator CallIf(object args)
        {
            return SubCall(new EvaluatorIf(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a cond evaluator.
        /// </summary>
        /// <param name="args">The cond to evaluate.</param>
        /// <returns>The step to execute.</returns>
        protected Evaluator CallReduceCond(object args)
        {
            return SubCall(new EvaluatorReduceCond(this.Interp, this, args, this.Env));
        }

        /// <summary>
        /// Create a closure evaluator.
        /// </summary>
        /// <param name="args">The closure to evaluate.</param>
        /// <param name="f">The enclosing closure.</param>
        /// <returns>The step to execute.</returns>
        protected Evaluator CallClosure(object args, Closure f)
        {
            return SubCall(new EvaluatorClosure(Interp, this, args, Env, f));
        }

        /// <summary>
        /// Create a list evaluator.
        /// </summary>
        /// <param name="args">The list to evaluate.</param>
        /// <returns>The step to execute.</returns>

        protected Evaluator CallList(object args)
        {
            return SubCall(new EvaluatorList(Interp, this, args, Env));
        }

        /// <summary>
        /// Create a apply evaluator.
        /// </summary>
        /// <param name="args">The application to evaluate.</param>
        /// <param name="fn">The proc to apply.</param>
        /// <returns>The step to execute.</returns>
        protected Evaluator CallApplyProc(object args, object fn)
        {
            return SubCall(new EvaluatorApplyProc(Interp, this, args, Env, fn));
        }
    }
}
