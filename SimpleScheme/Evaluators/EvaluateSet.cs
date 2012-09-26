// <copyright file="EvaluateSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
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
            : base(InitialStep, expr, env, caller)
        {
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
            SchemeObject lhs = First(expr);
            SchemeObject rhs = Second(expr);
            if (!(lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Set: first argument must be a symbol.  Got: ""{0}""", lhs), null);
            }

            return new EvaluateSet(expr, env, caller, lhs, rhs);
        }
        #endregion

        #region Steps
        /// <summary>
        /// Evaluate the second expression (rhs).
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Code to evaluate the second expression.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateSet)s;
            s.Pc = SetStep;
            return EvaluateExpression.Call(step.rhs, s.Env, s);
        }

        /// <summary>
        /// Back here after evaluation.  Assign the result to the variable in the environment
        ///   named by the first part of the expression.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>Returns to caller.</returns>
        private static Evaluator SetStep(Evaluator s)
        {
            var step = (EvaluateSet)s;
            if (!(step.lhs is Symbol))
            {
                ErrorHandlers.SemanticError(string.Format(@"Attempt to set a non-symbol: ""{0}""", step.lhs.ToString(true)), step.lhs);
            }

            s.Env.Set((Symbol)step.lhs, s.ReturnedExpr);
            Evaluator caller = s.Caller;
            caller.ReturnedExpr = Undefined.Instance;
            return caller;
        }
        #endregion
    }
}