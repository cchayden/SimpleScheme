#define OLD
// <copyright file="EvaluateProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    using Obj = System.Object;

    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public class EvaluateProc : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-proc";

        /// <summary>
        /// The proc or primitive to apply.
        /// </summary>
        private readonly Procedure fn;

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
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
        protected EvaluateProc(object args, Environment env, Stepper caller, Procedure fn, bool evaluate)
            : base(args, env, caller)
        {
            this.fn = fn;
            if (evaluate)
            {
                ContinueHere(EvalArgsStep);
            }
            else
            {
                this.UpdateReturnedExpr(args); // as if EvalArgs returned this, the unevaluated args
                ContinueHere(ApplyStep);
            }
            IncrementCounter(counter);
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
        public static Stepper Call(Procedure fn, Obj args, Environment env, Stepper caller)
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
        public static Stepper CallQuoted(Procedure fn, Obj args, Environment env, Stepper caller)
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
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Next action to evaluate the args.</returns>
        private static Stepper EvalArgsStep(Stepper s)
        {
            return EvaluateList.Call(s.Expr, s.Env, s.ContinueHere(ApplyStep));
        }

        /// <summary>
        /// Back here after args have been evaluated.  
        /// Apply the proc to the evaluated args.  
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The result, or the next step to obtain it.</returns>
        private static Stepper ApplyStep(Stepper s)
        {
            Procedure fn = ((EvaluateProc)s).fn;
            if (s.Interp.Trace)
            {
                s.Caller.Interp.CurrentOutputPort.WriteLine(
                    String.Format("{0}: ({1} {2})", StepperName, fn.ProcedureName, List.First(s.ReturnedExpr)));
            }

            // Pass s.Caller to return to the caller rather than to here, since there is
            //  nothing left to do.
            return fn.Apply(s.ReturnedExpr, s.Caller);
        }
        #endregion
    }
}