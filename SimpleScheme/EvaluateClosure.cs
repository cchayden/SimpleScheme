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
        /// Initializes a new instance of the EvaluateClosure class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="f">The closure to evaluate</param>
        private EvaluateClosure(Stepper parent, object expr, Environment env, Closure f)
            : base(parent, expr, env)
        {
            this.f = f;
            this.Pc = this.EvaluateArgsStep;
            this.trace = false;
            IncrementCounter("closure");
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
            this.Pc = this.EvalBodyInEnvironmentStep;
            return EvaluateList.Call(this, Expr);
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
                Console.WriteLine("({0} {1})", this.f.Name, List.First(ReturnedExpr));
            }

            this.Env = new Environment(this.f.FormalParameters, ReturnedExpr, this.f.Env);
            this.Pc = this.ReturnStep;
            return EvaluatorMain.Call(this, this.f.Body);
        }
    }
}