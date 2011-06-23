// <copyright file="EvaluateApplyProc.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate args and apply a proc to it.
    /// </summary>
    public sealed class EvaluateApplyProc : Stepper
    {
        /// <summary>
        /// The proc or primitive to apply.
        /// </summary>
        private readonly object fn;

        /// <summary>
        /// Initializes a new instance of the EvaluateApplyProc class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The function to apply.</param>
        private EvaluateApplyProc(Stepper parent, object expr, Environment env, object fn)
            : base(parent, expr, env)
        {
            this.fn = fn;
            this.Pc = this.InitialStep;
            IncrementCounter("apply-proc");
        }

        /// <summary>
        /// Call apply proc evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="fn">The function to apply.</param>
        /// <returns>The apply proc evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr, object fn)
        {
            return new EvaluateApplyProc(caller, expr, caller.Env, fn);
        }

        /// <summary>
        /// Begin by evaluating all the arguments.
        /// </summary>
        /// <returns>Next action to evaluate the args.</returns>
        private Stepper InitialStep()
        {
            this.Pc = this.ApplyStep;
            return EvaluateList.Call(this, Expr);
        }

        /// <summary>
        /// Back here after args have been evaluated.  Apply the proc to the
        ///   evaluated args.  If there is a result return it, otherwise, let the
        ///   proc proceed.
        /// </summary>
        /// <returns>The result, or the next step to obtain it.</returns>
        private Stepper ApplyStep()
        {
            this.Pc = this.ReturnStep;
            return Procedure.Proc(this.fn).Apply(this, ReturnedExpr);
        }

        /// <summary>
        /// Back from applying the proc.  Get the result and return it to the caller.
        /// </summary>
        /// <returns>Control returns to the caller.</returns>
        private Stepper ReturnStep()
        {
            return ReturnFromStep(ReturnedExpr);
        }
    }
}