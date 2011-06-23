// <copyright file="EvaluatorCallWithInputFile.cs" company="Charles Hayden">
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
        /// Open a file for input.
        /// </summary>
        /// <param name="filename">The filename of the file to open.</param>
        /// <returns>The input port, used for reading.</returns>
        public static InputPort OpenInputFile(object filename)
        {
            try
            {
                return new InputPort(new StreamReader(StringUtils.AsString(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (InputPort)Error("No such file: " + StringUtils.AsString(filename));
            }
            catch (IOException ex)
            {
                return (InputPort)Error("IOException: " + ex.Message);
            }
        }

        /// <summary>
        /// Evaluate a call-with-input-file expressions
        /// </summary>
        private class EvaluatorCallWithInputFile : Stepper
        {
            /// <summary>
            /// The input port to be used during the evaluation.
            /// </summary>
            private InputPort port;

            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorCallWithInputFile class.
            /// </summary>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorCallWithInputFile(Stepper parent, object expr, Environment env)
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
                            this.port = OpenInputFile(First(Expr));
                            object z = Procedure.Proc(Second(Expr)).Apply(this, List(this.port));
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

                    return EvalError("CallWithInputFile: program counter error");
                }
            }
        }
    }
}
