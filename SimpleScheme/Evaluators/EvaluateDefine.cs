// <copyright file="EvaluateDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;

    //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
    //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
    //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>

    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    internal sealed class EvaluateDefine : Evaluator
    {
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper initialStep = GetStepper("InitialStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper storeDefineStep = GetStepper("StoreDefineStep");

        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-define");
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateDefine class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateDefine(SchemeObject expr, Environment env, Evaluator caller)
            : base(initialStep, expr, env, caller, counter)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(counter >= 0);
        }
        #endregion

        #region Call
        /// <summary>
        /// Call a define evaluator.
        /// Handle the two forms of define.
        /// In the first case, just save the lambda and return.
        /// This is what would result if we prepend "lambda" and call EvaluateExpression.
        /// In the second case, we need create an evaluator to evaluate the expression.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The define evaluator.</returns>
        internal static Evaluator Call(SchemeObject expr, Environment env, Evaluator caller)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            if (First(expr) is Pair)
            {
                // Defun case -- create a lambda and bind it to the variable.
                var symbol = First(First(expr));
                if (!(symbol is Symbol))
                {
                    ErrorHandlers.SemanticError(string.Format(@"Attempt to define a non-symbol: ""{0}""", symbol.ToString(true)), null);
                }

                env.Define((Symbol)symbol, Lambda.New(Rest(First(expr)), Rest(expr), env));
                caller.ReturnedExpr = Undefined.Instance;
                return caller;
            }

            return new EvaluateDefine(expr, env, caller);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Start by evaluating the expression.
        /// </summary>
        /// <returns>Continue by evaluating the expression.</returns>
        protected override Evaluator InitialStep()
        {
            this.Pc = storeDefineStep;
            return EvaluateExpression.Call(Second(this.Expr), this.Env, this);
        }

        /// <summary>
        /// Back from expression evaluation.  Store the result as the value of the symbol
        /// </summary>
        /// <returns>Execution continues in the caller.</returns>
        protected override Evaluator StoreDefineStep()
        {
            this.Env.Define((Symbol)First(this.Expr), this.ReturnedExpr);
            this.Caller.ReturnedExpr = Undefined.Instance;
            return this.Caller;
        }

        #endregion
    }
}