// <copyright file="EvaluatorCallWithOutputFile.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
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
        /// Evaluate a call-with-output-file expressions
        /// </summary>
        private class EvaluatorCallWithOutputFile : Stepper
        {
            private PrintWriter p;

            /// <summary>
            /// Initializes a new instance of the Stepper.EvaluatorCallWithOutputFile class.
            /// </summary>
            /// <param name="interp">The interpreter.</param>
            /// <param name="parent">The parent.  Return to this when done.</param>
            /// <param name="expr">The expression to evaluate.</param>
            /// <param name="env">The evaluation environment</param>
            public EvaluatorCallWithOutputFile(Scheme interp, Stepper parent, object expr, Environment env)
                : base(interp, parent, expr, env)
            {
            }

            /// <summary>
            /// Evaluate an if expression.
            /// </summary>
            /// <returns>The next step to execute.</returns>
            public override Stepper EvalStep()
            {
                switch (Pc)
                {
                    case 0:
                        Pc = 1;
                        p = OpenOutputFile(First(Expr));
                        Procedure proc = Procedure.Proc(Second(Expr));
                        object z = proc.Apply(Interp, this, List(p));
                        if (z is Stepper)
                        {
                            return SubCall((Stepper)z);
                        }

                        return SubContinue(z);

                    case 1:
                        if (p != null)
                        {
                            p.Close();
                        }

                        return SubReturn(this.ReturnedExpr);
                }

                return EvalError("CallWithOutputFile: program counter error");
            }

        }

        /// <summary>
        /// Open a file for output.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <returns>The output port, used for writing.</returns>
        public static PrintWriter OpenOutputFile(object filename)
        {
            try
            {
                return new PrintWriter(new StreamWriter(Stringify(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (PrintWriter)Error("No such file: " + Stringify(filename));
            }
            catch (IOException ex)
            {
                return (PrintWriter)Error("IOException: " + ex.Message);
            }
        }
    }
}
