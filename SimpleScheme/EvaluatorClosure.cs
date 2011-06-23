// <copyright file="EvaluatorClosure.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate a closure
    /// </summary>
    public class EvaluatorClosure : Stepper
    {
        /// <summary>
        /// The closure to apply.
        /// </summary>
        private readonly Closure f;

        /// <summary>
        /// Initializes a new instance of the EvaluatorClosure class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="f">The closure to evaluate</param>
        private EvaluatorClosure(Stepper parent, object expr, Environment env, Closure f)
            : base(parent, expr, env)
        {
            this.f = f;
        }

        /// <summary>
        /// Creates a closure evaluator
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="f">The closure to evaluate</param>
        /// <returns>The closure evaluator..</returns>
        public static EvaluatorClosure New(Stepper parent, object expr, Environment env, Closure f)
        {
            return new EvaluatorClosure(parent, expr, env, f);
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
                    return CallList(Expr);

                case PC.Step1:
                    return SubReturn(this.f.Body, new Environment(this.f.FormalParameters, ReturnedExpr, this.f.Env));
            }

            return EvalError("Closure: program counter error");
        }
    }
}