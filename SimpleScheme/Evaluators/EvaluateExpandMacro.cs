// <copyright file="EvaluateExpandMacro.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Expand a macro.
    /// </summary>
    internal sealed class EvaluateExpandMacro : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-expand-macro");

        /// <summary>
        /// The macro to expand.
        /// </summary>
        private readonly Macro fn;
        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the EvaluateExpandMacro class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="fn">The macro to expand.</param>
        private EvaluateExpandMacro(SchemeObject expr, Environment env, Evaluator caller, Macro fn)
            : base(OpCode.Expand, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(fn != null);
            Contract.Requires(counter >= 0);
            this.fn = fn;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call an expand evaluator.
        /// This comes here with a macro (fn) and a set of macro arguments (args).
        /// The args are unevaluated.  
        /// The macro is applied to the unevaluated arguments, yielding an expanded program.
        /// Then the result is evaluated as an expression.
        /// </summary>
        /// <param name="fn">The macro to expand.</param>
        /// <param name="args">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The expand evaluator.</returns>
        internal static Evaluator Call(Macro fn, SchemeObject args, Environment env, Evaluator caller)
        {
            Contract.Requires(fn != null);
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return new EvaluateExpandMacro(args, env, caller, fn);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Apply the macro to the arguments, expanding it.  
        /// </summary>
        /// <returns>The step to evaluate the expanded macro.</returns>
        protected override Evaluator ExpandStep()
        {
            this.Pc = OpCode.Evaluate;
            return this.fn.Apply(this.Expr, this, this);
        }

        /// <summary>
        /// Back here after macro is expanded.  Evaluate the result.
        /// Before doing that, clean the expanded result.
        /// Cleaning removes any cached information from the symbols, because they may not be
        ///   evaluated at the same nesting level each time.
        /// </summary>
        /// <returns>Return to caller with the expanded macro.</returns>
        protected override Evaluator EvaluateStep()
        {
            SchemeObject obj = this.ReturnedExpr;
            obj.Clean();
            return EvaluateExpression.Call(obj, this.Env, this.Caller);
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.fn != null);
        }
        #endregion
    }
}