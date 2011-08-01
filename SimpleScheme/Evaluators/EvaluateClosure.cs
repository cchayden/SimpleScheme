﻿// <copyright file="EvaluateClosure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a closure
    /// </summary>
    public sealed class EvaluateClosure : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        public const string StepperName = "evaluate-closure";

        /// <summary>
        /// The closure to apply.
        /// </summary>
        private readonly Closure f;

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateClosure class.
        /// </summary>
        /// <param name="f">The closure to evaluate</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateClosure(Closure f, Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            this.f = f;
            ContinueHere(EvaluateArgsStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Calls a closure evaluator
        /// </summary>
        /// <param name="f">The closure to evaluate</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The environment to make the expression in.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The closure evaluator..</returns>
        public static Stepper Call(Closure f, Obj expr, Environment env, Stepper caller)
        {
            return new EvaluateClosure(f, expr, env, caller);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Trace information for the step.
        /// Print the closure name.
        /// </summary>
        /// <returns>Info to print for the trace.</returns>
        public override string TraceInfo()
        {
            if (base.TraceInfo() == null)
            {
                return null;
            }

            return TypePrimitives.TypeName(this) + " {" + this.f.ProcedureName + "}";
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate a closure, in other words execute the function that it defines in the 
        /// appropriate environment.  Start by evaluating the list of expressions.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The result of evaluating the argument list.</returns>
        private static Stepper EvaluateArgsStep(Stepper s)
        {
            return EvaluateList.Call(s.Expr, s.Env, s.ContinueHere(EvalBodyInEnvironmentStep));
        }

        /// <summary>
        /// Back here after evaluating the args, now apply the closure itself.
        /// Create a new environment matching the formal parameters up with the evaluated arguments, and link
        ///   back to the caller's environment.  Then evaluate the closure body.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step to evaluate the closure.</returns>
        private static Stepper EvalBodyInEnvironmentStep(Stepper s)
        {
            EvaluateClosure step = (EvaluateClosure)s;
            if (s.Caller.Interp.Trace)
            {
                s.Caller.Interp.CurrentOutputPort.WriteLine(
                    String.Format("evaluate-closure: ({0} {1})", step.f.ProcedureName, List.First(s.ReturnedExpr)));
            }

            return step.f.Apply(s.ReturnedExpr, s.Caller);
        }
        #endregion
    }
}