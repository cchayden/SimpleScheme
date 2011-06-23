// <copyright file="EvaluateClosure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Evaluate a closure
    /// </summary>
    public sealed class EvaluateClosure : Stepper
    {
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
        private const string StepperName = "closure";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

      /// <summary>
        /// Initializes a new instance of the EvaluateClosure class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="f">The closure to evaluate</param>
        private EvaluateClosure(Stepper caller, object expr, Environment env, Closure f)
            : base(caller, expr, env)
        {
            this.f = f;
            ContinueHere(this.EvaluateArgsStep);
            this.trace = false;
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
        }

        /// <summary>
        /// Calls a closure evaluator
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="f">The closure to evaluate</param>
        /// <returns>The closure evaluator..</returns>
        public static Stepper Call(Stepper caller, object expr, Closure f)
        {
            return new EvaluateClosure(caller, expr, caller.Env, f);
        }

        /// <summary>
        /// Evaluate a closure, in other words execute the function that it defines in the 
        /// appropriate environment.  Start by evaluating the list of expressions.
        /// </summary>
        /// <returns>The result of evaluating the argument list.</returns>
        private Stepper EvaluateArgsStep()
        {
            return EvaluateList.Call(ContinueHere(this.EvalBodyInEnvironmentStep), Expr);
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
    }
}