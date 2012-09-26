// <copyright file="EvaluateSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Evaluate a set! expression.
    /// </summary>
    //// <r4rs section="4.1.6">(set <variable> <expression>)</r4rs>
    internal sealed class EvaluateSet : Evaluator
    {
        #region Fields
        /// <summary>
        /// The variable of the assignment.
        /// </summary>
        private SchemeObject lhs;

        /// <summary>
        /// The expression of the assignment.
        /// </summary>
        private SchemeObject rhs;
        #endregion

        #region Call
        /// <summary>
        /// Calls a set evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The set evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            var lhs = First(expr);
            if (!(lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Set: first argument must be a symbol.  Got: ""{0}""", lhs));
            }

            return New(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the second expression (rhs).
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator InitialStep()
        {
            this.Pc = OpCode.Set;
            return EvaluateExpression.Call(this.rhs, this.Env, this);
        }

        /// <summary>
        /// Back here after evaluation.  Assign the result to the variable in the environment
        ///   named by the first part of the expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator SetStep()
        {
            if (!(this.lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Attempt to set a non-symbol: ""{0}""", this.lhs.ToString(true)));
            }

            this.Env.Set((Symbol)this.lhs, this.ReturnedExpr);
            return this.ReturnFromEvaluator(Undefined.Instance);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateSet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateSet New(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return GetInstance<EvaluateSet>().Initialize(expr, env, caller);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateSet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>Newly initialized evaluator.</returns>
        private EvaluateSet Initialize(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            this.lhs = First(expr);
            this.rhs = Second(expr);
            Initialize(OpCode.Initial, expr, env, caller);
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
            Contract.Invariant(this.degenerate || this.lhs != null);
            Contract.Invariant(this.degenerate || this.rhs != null);
        }
        #endregion
    }
}