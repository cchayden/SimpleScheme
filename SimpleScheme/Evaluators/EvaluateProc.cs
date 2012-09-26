// <copyright file="EvaluateProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
        /// <param name="evaluate">If true, evaluate the args.  If false, do not evaluate them.</param>
        private EvaluateProc(Procedure fn, SchemeObject args, Environment env, Evaluator caller, bool evaluate)
            : base(EvalArgsStep, args, env, caller, counter)
        {
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
            return new EvaluateProc(fn, args, env, caller, true);
        }

        /// <summary>
        /// Call apply proc evaluator, but do not evaluate arguments.
        /// In this case, we can avoid having to instantiate an instance.
        /// </summary>
        /// <param name="fn">The function to apply.</param>
        /// <param name="args">The arguments to pass to fn.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The proc evaluator.</returns>
        internal static Evaluator CallQuoted(Procedure fn, SchemeObject args, Environment env, Evaluator caller)
        {
            return fn.Apply(args, null, caller, caller);
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
        /// Begin by evaluating all the arguments.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Next action to evaluate the args.</returns>
        private static Evaluator EvalArgsStep(Evaluator s)
        {
            s.Pc = ApplyStep;
// TODO fold EvaluateList steps into here
            return EvaluateList.Call(s.Expr, s.Env, s);
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
                    string.Format("evaluate-proc: {0} applied to {1}", fn.ProcedureName, s.ReturnedExpr));
            }
#endif

            // Pass s.Caller to return to the caller rather than to here, since there is
            //  nothing left to do.
            return fn.Apply(s.ReturnedExpr, null, s.Caller, s);
        }
        #endregion
    }
}