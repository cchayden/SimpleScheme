﻿// <copyright file="EvaluateProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    internal sealed class EvaluateProc : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-proc");

        /// <summary>
        /// The proc or primitive to apply.
        /// </summary>
        private readonly Procedure fn;
        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the EvaluateProc class.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="args">The arguments to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateProc(Procedure fn, SchemeObject args, Environment env, Evaluator caller)
            : base(OpCode.Apply, args, env, caller, counter)
        {
            Contract.Requires(fn != null);
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            this.fn = fn;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call apply proc evaluator after evaluating args.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="args">The arguments to evaluate, the pass to fn.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The proc evaluator.</returns>
        internal static Evaluator Call(Procedure fn, SchemeObject args, Environment env, Evaluator caller)
        {
            Contract.Requires(fn != null);
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Ensures(Contract.Result<Evaluator>() != null);
            var evalProc = new EvaluateProc(fn, args, env, caller);
            return EvaluateList.Call(evalProc.Expr, evalProc.Env, evalProc);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Provide TraceInfo: the name and the proc to execute.
        /// </summary>
        /// <returns>Trace info.</returns>
        internal override string TraceInfo()
        {
            string info = base.TraceInfo();
            return info == null ? null : info + " " + this.fn;
        }
        #endregion

        #region Steps
        /// <summary>
        /// Back here after args have been evaluated.  
        /// Apply the proc to the evaluated args.  
        /// </summary>
        /// <returns>The result, or the next step to obtain it.</returns>
        protected override Evaluator ApplyStep()
        {
#if Diagnostics
            if (this.Interp.Trace)
            {
                this.Caller.Interp.CurrentOutputPort.WriteLine(
                    string.Format("evaluate-proc: {0} applied to {1}", this.fn.ProcedureName, this.ReturnedExpr));
            }
#endif

            // Pass this.Caller to return to the caller rather than to here, since there is
            //  nothing left to do.
            return this.fn.Apply(this.ReturnedExpr, this.Caller, this);
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