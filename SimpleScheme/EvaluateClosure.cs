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
        }

        /// <summary>
        /// Calls a closure evaluator
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="f">The closure to evaluate</param>
        /// <returns>The closure evaluator..</returns>
        public static EvaluateClosure Call(Stepper caller, object expr, Closure f)
        {
            return new EvaluateClosure(caller, expr, caller.Env, f);
        }

        /// <summary>
        /// Evaluate a set! expression.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            switch (this.Pc)
            {
                case PC.Initial:
                    Pc = PC.Step1;
                    return EvaluateList.Call(this, Expr);

                case PC.Step1:
                    //Console.WriteLine("({0} {1})", this.f.Name, List.First(ReturnedExpr));
                    return ReturnFromStep(this.f.Body, new Environment(this.f.FormalParameters, ReturnedExpr, this.f.Env));
            }

            return ErrorHandlers.EvalError("Closure: program counter error");
        }
    }
}