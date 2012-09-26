// <copyright file="EvaluateDefine.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    //// <r4rs section="5.2">(define <variable> <expression>)</r4rs>
    //// <r4rs section="5.2">(define (<variable> <formals>) <body>)</r4rs>
    //// <r4rs section="5.2">(define (<variable> . <formal>) <body>)</r4rs>

    /// <summary>
    /// Evaluate a define expression.
    /// </summary>
    public sealed class EvaluateDefine : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-define";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateDefine class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateDefine(ISchemeObject expr, Environment env, Evaluator caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
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
        public static Evaluator Call(ISchemeObject expr, Environment env, Evaluator caller)
        {
            if (List.First(expr) is Pair)
            {
                // Defun case -- create a lambda and bind it to the variable.
                var symbol = List.First(List.First(expr));
                if (!(symbol  is Symbol))
                {
                    ErrorHandlers.SemanticError(string.Format(@"Attempt to define a non-symbol: ""{0}""", Printer.AsString(symbol)));
                }

                env.Define(symbol.AsSymbol(), Lambda.New(List.Rest(List.First(expr)), List.Rest(expr), env));
                return caller.UpdateReturnValue(Undefined.Instance);
            }

            return new EvaluateDefine(expr, env, caller);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Start by evaluating the expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Continue by evaluating the expression.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            return EvaluateExpression.Call(List.Second(s.Expr), s.Env, s.ContinueHere(StoreDefineStep));
        }

        /// <summary>
        /// Back from expression evaluation.  Store the result as the value of the symbol
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Execution continues in the caller.</returns>
        private static Evaluator StoreDefineStep(Evaluator s)
        {
            var symbol = List.First(s.Expr);
            if (!(symbol is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Attempt to store to a non-symbol: ""{0}""", Printer.AsString(symbol)));
            }

            s.Env.Define(symbol.AsSymbol(), s.ReturnedExpr);
            return s.ReturnUndefined();
        }

        #endregion
    }
}