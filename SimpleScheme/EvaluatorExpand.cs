// <copyright file="EvaluatorExpand.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Expand a macro.
    /// </summary>
    public class EvaluatorExpand : Stepper
    {
        /// <summary>
        /// The macro to expand.
        /// </summary>
        private readonly Macro fn;

        /// <summary>
        /// Initializes a new instance of the EvaluatorExpand class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The macro to expand.</param>
        private EvaluatorExpand(Stepper parent, object expr, Environment env, Macro fn)
            : base(parent, expr, env)
        {
            this.fn = fn;
        }

        /// <summary>
        /// Creates a new expand evaluator.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="fn">The macro to expand.</param>
        /// <returns>The expand evaluator.</returns>
        public static EvaluatorExpand New(Stepper parent, object expr, Environment env, Macro fn)
        {
            return new EvaluatorExpand(parent, expr, env, fn);
        }

        /// <summary>
        /// Expand a macro
        /// </summary>
        /// <returns>The next step to execute.</returns>
        public override Stepper RunStep()
        {
            switch (Pc)
            {
                case PC.Initial:
                    object expanded = this.fn.Apply(this, Expr);
                    if (expanded is Stepper)
                    {
                        return GoToStep(PC.Step1, (Stepper)expanded);
                    }

                    return EvalError("Expand: should not get here");

                case PC.Step1:
                    return SubReturn(ReturnedExpr);
            }

            return EvalError("Expand: program counter error");
        }
    }
}