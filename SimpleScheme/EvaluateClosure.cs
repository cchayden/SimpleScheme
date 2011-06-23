// <copyright file="EvaluateClosure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a closure
    /// </summary>
    internal sealed class EvaluateClosure : Stepper
    {
        #region Fields
        /// <summary>
        /// The closure to apply.
        /// </summary>
        private readonly Closure f;

        /// <summary>
        /// Indicates whether to trace.
        /// </summary>
        private readonly bool trace;

        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "evaluate-closure";

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
            ContinueHere(this.EvaluateArgsStep);
            this.trace = false;
            IncrementCounter(counter);
        }
        #endregion

        #region Fields
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Calls a closure evaluator
        /// </summary>
        /// <param name="f">The closure to evaluate</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The closure evaluator..</returns>
        internal static Stepper Call(Closure f, Obj expr, Stepper caller)
        {
            return new EvaluateClosure(f, expr, caller.Env, caller);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Trace information for the step.
        /// Print the closure name in addition to args
        /// </summary>
        /// <returns>Info to print for the trace.</returns>
        internal override string TraceInfo()
        {
            if (base.TraceInfo() == null)
            {
                return null;
            }

            return this.Name + " {" + this.f.Name + "}";
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Evaluate a closure, in other words execute the function that it defines in the 
        /// appropriate environment.  Start by evaluating the list of expressions.
        /// </summary>
        /// <returns>The result of evaluating the argument list.</returns>
        private Stepper EvaluateArgsStep()
        {
            return EvaluateList.Call(Expr, ContinueHere(this.EvalBodyInEnvironmentStep));
        }

        /// <summary>
        /// Back here after evaluating the args, now apply the closure itself.
        /// Create a new environment matching the formal parameters up with the evaluated arguments, and link
        ///   back to the caller's environment.  The evaluate the closure body.
        /// </summary>
        /// <returns>The next step to evaluate the closure.</returns>
        private Stepper EvalBodyInEnvironmentStep()
        {
            if (this.trace)
            {
                Console.Out.WriteLine("({0} {1})", this.f.Name, First(ReturnedExpr));
            }

            this.ReplaceEnvironment(this.f.FormalParameters, ReturnedExpr, this.f.Env);
            return this.f.ApplyWithCurrentEnv(ContinueHere(this.ReturnStep));
        }
        #endregion
    }
}