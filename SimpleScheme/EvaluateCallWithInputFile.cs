// <copyright file="EvaluateCallWithInputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-input-file expressions
    /// </summary>
    public sealed class EvaluateCallWithInputFile : Stepper
    {
        /// <summary>
        /// The input port to be used during the evaluation.
        /// </summary>
        private InputPort port;

        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithInputFile class.
        /// </summary>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateCallWithInputFile(Stepper parent, object expr, Environment env)
            : base(parent, expr, env)
        {
        }

        /// <summary>
        /// Call a new evaluator with an input file
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The created evaluator.</returns>
        public static EvaluateCallWithInputFile Call(Stepper caller, object expr)
        {
            return new EvaluateCallWithInputFile(caller, expr, caller.Env);
        }

        /// <summary>
        /// Open a file for input.
        /// </summary>
        /// <param name="filename">The filename of the file to open.</param>
        /// <returns>The input port, used for reading.</returns>
        public static InputPort OpenInputFile(object filename)
        {
            try
            {
                return new InputPort(new StreamReader(SchemeString.AsString(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (InputPort)ErrorHandlers.Error("No such file: " + SchemeString.AsString(filename));
            }
            catch (IOException ex)
            {
                return (InputPort)ErrorHandlers.Error("IOException: " + ex.Message);
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
                        this.port = OpenInputFile(List.First(Expr));
                        object z = Procedure.Proc(List.Second(Expr)).Apply(this, List.MakeList(this.port));
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

                return ErrorHandlers.EvalError("CallWithInputFile: program counter error");
            }
        }
    }
}
