#define OLDxx
// <copyright file="Eval.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Template for an evaluation procedure.
    /// </summary>
    /// <returns>The Evaluator to execute next.</returns>
    //public delegate Evaluator Evaluator();

    /// <summary>
    /// Evaluates expressions step by step.
    /// </summary>
    public abstract class Evaluator : SchemeUtils
    {
        protected const int PcReturn = -1;

        /// <summary>
        /// Initializes a new instance of the Evaluator class.
        /// </summary>
        /// <param name="interp">The scheme interpreter.</param>
        /// <param name="parent">The parent evaluator.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluator environment.</param>
        protected Evaluator(Scheme interp, Evaluator parent, object expr, Environment env)
        {
            this.Interp = interp;
            this.Parent = parent;
            this.RetExpr = this.Expr = expr;
            this.RetEnv = this.Env = env;
            this.Pc = 0;
            this.Called = null;
        }

        protected Scheme Interp { get; private set; }
        protected Evaluator Parent { get; private set; }

        /// <summary>
        /// Gets or sets the expression being evaluated.  
        /// </summary>
        public object Expr { get; protected set; }

        /// <summary>
        /// Gets or sets the returned expression.
        /// This is valid after a call has completed, and holds the
        ///   returned result.
        /// </summary>
        public object RetExpr { get; protected set; }

        /// <summary>
        /// Gets or sets the evaluation environment.  After execution, this is the new environment.
        /// </summary>
        public Environment Env { get; protected set; }

        /// <summary>
        /// Gets or sets the returned environment
        /// </summary>
        public Environment RetEnv { get; protected set; }

        /// <summary>
        /// Gets or sets the Evaluators program counter.
        /// Used to sequence through multiple steps.
        /// </summary>
        protected int Pc { get; set;  }

        /// <summary>
        /// Gets or sets the called sub-evaluator.
        /// The evaluator that is called is stored, so that the returned value
        ///   can be extracted after it returns.
        /// </summary>
        protected Evaluator Called { get; set; }

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
        protected Evaluator SubEval(Evaluator toCall)
        {
            this.Called = toCall;
            return toCall;
        }

        /// <summary>
        /// Make a call to a sub-evaluator.
        /// When that returns, store the result and return.
        /// </summary>
        /// <param name="toCall">The evaluator to call.</param>
        /// <returns>The first step of the sub-evaluator.</returns>
        protected Evaluator SubEvalReturn(Evaluator toCall)
        {
            Pc = PcReturn;
            return SubEval(toCall);
        }

        /// <summary>
        /// Return immediately.
        /// </summary>
        /// <returns>The parent evaluator..</returns>
        protected Evaluator EvalReturn()
        {
            return this.Parent;
        }

        /// <summary>
        /// Continue executing in the existing evaluator.
        /// </summary>
        /// <returns></returns>
        protected Evaluator EvalContinue()
        {
            Called = null;
            return this;
        }

        /// <summary>
        /// Recursive Evaluator call.
        /// </summary>
        /// <param name="args">The expression to evaluate.</param>
        /// <returns>The next evaluation step.</returns>
        protected Evaluator CallEval(object args)
        {
            return SubEval(new EvaluatorMain(this.Interp, this, args, this.Env));
        }
    }
}
