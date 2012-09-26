// <copyright file="EvaluateSequence.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a sequence by evaluating each member and returning the last value.
    /// </summary>
   //// <r4rs section="4.2.3">(begin <expression1> <expression2> ...)</r4rs>
    internal sealed class EvaluateSequence : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-sequence");
        #endregion

        #region Call
        /// <summary>
        /// Call the sequence evaluator.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The environment to evaluate in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The sequence evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return Rest(expr) is EmptyList ?
                EvaluateExpression.Call(First(expr), env, caller) :
                New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Initial step: to see if we are done.
        /// If not, evaluate the next expression.
        /// If we are, evaluate and return the last expr.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvalExprStep()
        {
            if (Rest(this.Expr) is EmptyList)
            {
                // On the last expr in the sequence, return directly to the caller.
                // This is *crucial* for tail recursion.
                // If this instead continues to a "DoneStep" here then each
                //   EvaluateSequence and each environment will be stacked up.
                SchemeObject f = First(this.Expr);
                Environment ev = this.Env;
                Evaluator c = this.Caller;
                this.Reclaim();
                return EvaluateExpression.Call(f, ev, c);
            }

            this.Pc = OpCode.Loop;
            return EvaluateExpression.Call(First(this.Expr), this.Env, this);
        }

        /// <summary>
        /// Comes back here after expression evaluation.  Loop back and evaluate another.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator LoopStep()
        {
            this.Expr = Rest(this.Expr);

            // instead of taking another step, do EvalExpr in line
            return this.EvalExprStep();
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateSequence class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateSequence New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateSequence>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateSequence class.
        /// </summary>
        /// <param name="expr">The expressions to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Newly initialized evaluator.</returns>
        private EvaluateSequence Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
            Initialize(OpCode.EvalExpr, expr, env, caller, counter);
            return this;
        }
        #endregion
    }
}