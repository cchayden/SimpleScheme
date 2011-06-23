// <copyright file="EvaluateTime.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Evaluate an expression while timing it.
    /// This may evaluate the expression multiple times.
    /// </summary>
    public sealed class EvaluateTime : EvaluateTimeBase
    {
        /// <summary>
        /// Initializes a new instance of the EvaluateTime class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateTime(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
            IncrementCounter("time");
        }

        /// <summary>
        /// Call a timed evaluator.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The timed evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
        {
            return new EvaluateTime(caller, expr, caller.Env);
        }

        /// <summary>
        /// Evaluate the given expression.  
        /// This evaluates the expression that is being timed.
        /// Test to see if we are done.
        /// </summary>
        /// <returns>If done, the result.  Otherwise, continue to next step.</returns>
        protected override Stepper Step1()
        {
            this.Pc = this.Step2;
            return EvaluatorMain.Call(this, List.First(Expr));
        }
    }
}
