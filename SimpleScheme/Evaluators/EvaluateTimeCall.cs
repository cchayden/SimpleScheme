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
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-time-call");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateTimeCall class.
        /// </summary>
        /// <param name="proc">The expression to evaluate.</param>
        /// <param name="count">The number of times to evaluate the expression.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateTimeCall(Procedure proc, int count, Environment env, Evaluator caller)
            : base(proc, count, env, caller, counter)
        {
            Contract.Requires(proc != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
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
            return new EvaluateTimeCall(proc, n, env, caller);
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
        protected override Evaluator EvaluateStep()
        {
            this.Pc = OpCode.Done;
            Contract.Assume(this.Expr is Procedure);
            return ((Procedure)this.Expr).Apply(EmptyList.Instance, this, this);
        }
        #endregion
    }
}
