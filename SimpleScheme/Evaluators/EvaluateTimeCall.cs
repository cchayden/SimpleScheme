// <copyright file="EvaluateTimeCall.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate an expression while timing it..
    /// This can evaluate the expression multiple times.
    /// The timing itself is donein EvaluateTimebase.
    /// </summary>
    internal sealed class EvaluateTimeCall : EvaluateTimeBase
    {
        #region Initialize
        /// <summary>
        /// Initializes a new instance of the EvaluateTimeCall class.
        /// </summary>
        /// <param name="proc">The expression to evaluate.</param>
        /// <param name="count">The number of times to evaluate the expression.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateTimeCall Initialize(Procedure proc, int count, Environment env, Evaluator caller)
        {
            Contract.Requires(proc != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            base.Initialize(proc, count, env, caller);
            return this;
        }

        /// <summary>
        /// Creates and initializes a new instance of the EvaluateTimeCall class.
        /// </summary>
        /// <param name="proc">The expression to evaluate.</param>
        /// <param name="count">The number of times to evaluate the expression.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateTimeCall New(Procedure proc, int count, Environment env, Evaluator caller)
        {
            Contract.Requires(proc != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return new EvaluateTimeCall().Initialize(proc, count, env, caller);
        }
        #endregion

        #region Call
        /// <summary>
        /// Call a timed evaluator.
        /// </summary>
        /// <param name="proc">The proc to evaluate.</param>
        /// <param name="count">The number of times to evaluate the proc.</param>
        /// <param name="env">The environment to make the proc in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The timed evaluator.</returns>
        internal static Evaluator Call(Procedure proc, SchemeObject count, Environment env, Evaluator caller)
        {
            Contract.Requires(proc != null);
            Contract.Requires(count != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            int n = count is EmptyList ? 1 : Number.AsInt(count);
            return New(proc, n, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the given expression.  
        /// This evaluates the expression that is being timed.
        /// Test to see if we are done.
        /// Caller ensures that first arg is a procedure.
        /// </summary>
        /// <returns>If done, the result.  Otherwise, continue to next step.</returns>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator EvaluateStep()
        {
            this.Pc = OpCode.Done;
            Contract.Assume(this.Expr is Procedure);
            return ((Procedure)this.Expr).Apply(EmptyList.Instance, this);
        }
        #endregion
    }
}
