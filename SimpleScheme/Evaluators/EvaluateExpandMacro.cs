﻿// <copyright file="EvaluateExpandMacro.cs" company="Charles Hayden">
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
        /// The macro to expand.
        /// </summary>
        private Macro fn;
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
            return New(args, env, caller, fn);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Apply the macro to the arguments, expanding it.  
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator ExpandStep()
        {
            this.Pc = OpCode.Evaluate;
            return this.fn.Apply(this.Expr, this);
        }

        /// <summary>
        /// Back here after macro is expanded.  Evaluate the result.
        /// Before doing that, clean the expanded result.
        /// Cleaning removes any cached information from the symbols, because they may not be
        ///   evaluated at the same nesting level each time.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvaluateStep()
        {
            SchemeObject obj = this.ReturnedExpr;
            obj.Clean();
            Environment ev = this.Env;
            Evaluator c = this.Caller;
            this.Reclaim();
            return EvaluateExpression.Call(obj, ev, c);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateExpandMacro class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="fn">The macro to expand.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateExpandMacro New(SchemeObject expr, Environment env, Evaluator caller, Macro fn)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return new EvaluateExpandMacro().Initialize(expr, env, caller, fn);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateExpandMacro class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="fun">The macro to expand.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateExpandMacro Initialize(SchemeObject expr, Environment env, Evaluator caller, Macro fun)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(fun != null);
            this.fn = fun;
            Initialize(OpCode.Expand, expr, env, caller);
            return this;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.degenerate || this.fn != null);
        }
        #endregion
    }
}