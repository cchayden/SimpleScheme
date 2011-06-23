// <copyright file="EvaluatorCallWithOutputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Stepper contains all the individual evaluators
    /// </summary>
    public partial class Stepper
    {
        /// <summary>
        /// Open a file for output.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <returns>The output port, used for writing.</returns>
        public static OutputPort OpenOutputFile(object filename)
        {
            try
            {
                return new OutputPort(new StreamWriter(StringUtils.AsString(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (OutputPort)Error("No such file: " + StringUtils.AsString(filename));
            }
            catch (IOException ex)
            {
                return (OutputPort)Error("IOException: " + ex.Message);
            }
        }

        /// <summary>
        /// Evaluate a call-with-output-file expressions
        /// </summary>
        private class EvaluatorCallWithOutputFile : Stepper
        {
            /// <summary>
            /// The output port to use during evaluation.
            /// </summary>
            private OutputPort port;

            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorCallWithOutputFile class.
            /// </summary>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorCallWithOutputFile(Stepper parent, object expr, Environment env)
                : base(parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate an if expression.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper RunStep()
            {
                while (true)
                {
                    switch (Pc)
                    {
                        case PC.Initial:
                            this.port = OpenOutputFile(First(Expr));
                            Procedure proc = Procedure.Proc(Second(Expr));
                            object z = proc.Apply(this, List(this.port));
                            if (z is Stepper)
                            {
                                return GoToStep(PC.Step1, (Stepper)z);
                            }

                            Pc = PC.Step1;
                            ReturnedExpr = z;
                            continue;

                        case PC.Step1:
                            if (this.port != null)
                            {
                                this.port.Close();
                            }

                            return SubReturn(this.ReturnedExpr);
                    }

                    return EvalError("CallWithOutputFile: program counter error");
                }
            }
        }
    }
}
