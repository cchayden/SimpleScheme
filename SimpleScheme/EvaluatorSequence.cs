// <copyright file="EvaluatorSequence.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Evaluate a sequence by evaluating each member and returning the last value.
        /// </summary>
        private class EvaluatorSequence : Stepper
        {
            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorSequence class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorSequence(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate a sequence of objects, returning the last.
            /// This was a simple while loop that has been split in the middle.
            /// </summary>
            /// <returns>The next step.</returns>
            public override Stepper RunStep()
            {
                while (true)
                {
                    switch (this.Pc)
                    {
                        case PC.Initial:
                            if (Rest(this.Expr) == null)
                            {
                                return SubReturn(First(this.Expr));
                            }

                            this.Pc = PC.Step1;
                            return CallEval(First(this.Expr));

                        case PC.Step1:
                            this.Expr = Rest(this.Expr);
                            this.Pc = PC.Initial;
                            continue;
                    }

                    return EvalError("Sequence: program counter error");
                }
            }
        }
    }
}