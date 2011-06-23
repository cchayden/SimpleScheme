// <copyright file="EvaluateCallWithOutputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-output-file expressions
    /// </summary>
    public sealed class EvaluateCallWithOutputFile : Stepper
    {
        /// <summary>
        /// The output port to use during evaluation.
        /// </summary>
        private OutputPort port;

        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithOutputFile class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateCallWithOutputFile(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Create an evaluator with output file.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The created evaluator.</returns>
        public static EvaluateCallWithOutputFile Call(Stepper caller, object expr)
        {
            return new EvaluateCallWithOutputFile(caller, expr, caller.Env);
        }

        /// <summary>
        /// Open a file for output.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <returns>The output port, used for writing.</returns>
        public static OutputPort OpenOutputFile(object filename)
        {
            try
            {
                return new OutputPort(new StreamWriter(SchemeString.AsString(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (OutputPort)ErrorHandlers.Error("No such file: " + SchemeString.AsString(filename));
            }
            catch (IOException ex)
            {
                return (OutputPort)ErrorHandlers.Error("IOException: " + ex.Message);
            }
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
                        this.port = OpenOutputFile(List.First(Expr));
                        Procedure proc = Procedure.Proc(List.Second(Expr));
                        object z = proc.Apply(this, List.MakeList(this.port));
                        if (z is Stepper)
                        {
                            return GoToStep((Stepper)z, PC.Step1);
                        }

                        Pc = PC.Step1;
                        ReturnedExpr = z;
                        continue;

                    case PC.Step1:
                        if (this.port != null)
                        {
                            this.port.Close();
                        }

                        return ReturnFromStep(this.ReturnedExpr);
                }

                return ErrorHandlers.EvalError("CallWithOutputFile: program counter error");
            }
        }
    }
}
