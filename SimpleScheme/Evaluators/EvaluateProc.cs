﻿// <copyright file="EvaluateProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public sealed class EvaluateProc : Evaluator
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
        /// <param name="args">The arguments to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="fn">The function to apply.</param>
        /// <param name="evaluate">If true, evaluate the args.  If false, do not evaluate them.</param>
        private EvaluateProc(SchemeObject args, Environment env, Evaluator caller, Procedure fn, bool evaluate)
            : base(args, env, caller, counter)
        {
            this.fn = fn;
            if (evaluate)
            {
                this.ContinueAt(EvalArgsStep);
            }
            else
            {
                this.UpdateReturnValue(args); // as if EvalArgs returned this, the unevaluated args
                this.ContinueAt(ApplyStep);
            }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call apply proc evaluator after evaluating args.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="args">The arguments to evaluate, the pass to fn.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The proc evaluator.</returns>
        public static Evaluator Call(Procedure fn, SchemeObject args, Environment env, Evaluator caller)
        {
            return new EvaluateProc(args, env, caller, fn, true);
        }

        /// <summary>
        /// Call apply proc evaluator, but do not evaluate arguments.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="args">The arguments to pass to fn.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The proc evaluator.</returns>
        public static Evaluator CallQuoted(Procedure fn, SchemeObject args, Environment env, Evaluator caller)
        {
            return new EvaluateProc(args, env, caller, fn, false);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Provide TraceInfo: the name and the proc to execute.
        /// </summary>
        /// <returns>Trace info.</returns>
        public override string TraceInfo()
        {
            string info = base.TraceInfo();
            return info == null ? null : info + " " + this.fn;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Begin by evaluating all the arguments.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Next action to evaluate the args.</returns>
        private static Evaluator EvalArgsStep(Evaluator s)
        {
            return EvaluateList.Call(s.Expr, s.Env, s.ContinueAt(ApplyStep));
        }

        /// <summary>
        /// Back here after args have been evaluated.  
        /// Apply the proc to the evaluated args.  
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The result, or the next step to obtain it.</returns>
        private static Evaluator ApplyStep(Evaluator s)
        {
            Procedure fn = ((EvaluateProc)s).fn;
#if Diagnostics
            if (s.Interp.Trace)
            {
                s.Caller.Interp.CurrentOutputPort.WriteLine(
                    String.Format("evaluate-proc: {0} applied to {1}", fn.ProcedureName, s.ReturnedExpr));
            }
#endif

            // Pass s.Caller to return to the caller rather than to here, since there is
            //  nothing left to do.
            return fn.Apply(EnsureSchemeObject(s.ReturnedExpr), s.Caller);
        }
        #endregion
    }
}