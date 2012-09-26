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
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper initialStep = GetStepper("InitialStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper setStep = GetStepper("SetStep");

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-set");

        /// <summary>
        /// The variable of the assignment.
        /// </summary>
        private readonly SchemeObject lhs;

        /// <summary>
        /// The expression of the assignment.
        /// </summary>
        private readonly SchemeObject rhs;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the EvaluateSet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="lhs">The left hand side -- the variable to set.</param>
        /// <param name="rhs">The right hand side -- the new value.</param>
        private EvaluateSet(SchemeObject expr, Environment env, Evaluator caller, SchemeObject lhs, SchemeObject rhs)
            : base(initialStep, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(lhs != null);
            Contract.Requires(rhs != null);
            Contract.Requires(counter >= 0);
            this.lhs = lhs;
            this.rhs = rhs;
        }
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
            var rhs = Second(expr);
            if (!(lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Set: first argument must be a symbol.  Got: ""{0}""", lhs), lhs);
            }

            return new EvaluateSet(expr, env, caller, lhs, rhs);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the second expression (rhs).
        /// </summary>
        /// <returns>Code to evaluate the second expression.</returns>
        protected override Evaluator InitialStep()
        {
            this.Pc = setStep;
            return EvaluateExpression.Call(this.rhs, this.Env, this);
        }

        /// <summary>
        /// Back here after evaluation.  Assign the result to the variable in the environment
        ///   named by the first part of the expression.
        /// </summary>
        /// <returns>Returns to caller.</returns>
        protected override Evaluator SetStep()
        {
            if (!(this.lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Attempt to set a non-symbol: ""{0}""", this.lhs.ToString(true)), this.lhs);
            }

            this.Env.Set((Symbol)this.lhs, this.ReturnedExpr);
            Evaluator caller = this.Caller;
            Contract.Assert(caller != null);
            caller.ReturnedExpr = Undefined.Instance;
            return caller;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.lhs != null);
            Contract.Invariant(this.rhs != null);
        }
        #endregion
    }
}