﻿// <copyright file="EvaluateSet.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a set! expression.
    /// </summary>
    //// <r4rs section="4.1.6">(set <variable> <expression>)</r4rs>
    public sealed class EvaluateSet : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-set";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The variable of the assignment.
        /// </summary>
        private readonly Obj lhs;

        /// <summary>
        /// The expression of the assignment.
        /// </summary>
        private readonly Obj rhs;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the EvaluateSet class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateSet(Obj expr, Environment env, Stepper caller, Obj lhs, Obj rhs)
            : base(expr, env, caller)
        {
            this.lhs = lhs;
            this.rhs = rhs;
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Calls a set evaluator.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to evaluate the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The set evaluator.</returns>
        public static Stepper Call(Obj expr, Environment env, Stepper caller)
        {
            Obj lhs = List.First(expr);
            Obj rhs = List.Second(expr);
            if (!Symbol.Is(lhs))
            {
                ErrorHandlers.SemanticError("Set: first argument must be a symbol.  Got: " + lhs);
            }
            return new EvaluateSet(expr, env, caller, lhs, rhs);
        }

        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate the second expression (rhs).
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Code to evaluate the second expression.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateSet step = (EvaluateSet)s;
            return EvaluateExpression.Call(step.rhs, s.Env, s.ContinueHere(SetStep));
        }

        /// <summary>
        /// Back here after evaluation.  Assign the result to the variable in the environment
        ///   named by the first part of the expression.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>Returns to caller.</returns>
        private static Stepper SetStep(Stepper s)
        {
            EvaluateSet step = (EvaluateSet)s;
            s.Env.Set(step.lhs, s.ReturnedExpr);
            return s.ReturnUndefined();
        }
        #endregion
    }
}