﻿// <copyright file="EvaluateCallWithInputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-input-file expressions
    /// </summary>
    public class EvaluateCallWithInputFile : Stepper
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
        /// Create a new evaluator
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="parent">The parent.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        public static EvaluateCallWithInputFile New(object expr, Environment env, Stepper parent)
        {
            return new EvaluateCallWithInputFile(parent, expr, env);
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
                return (InputPort)Error("No such file: " + SchemeString.AsString(filename));
            }
            catch (IOException ex)
            {
                return (InputPort)Error("IOException: " + ex.Message);
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
                        this.port = OpenInputFile(First(Expr));
                        object z = Procedure.Proc(Second(Expr)).Apply(this, List(this.port));
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

                return EvalError("CallWithInputFile: program counter error");
            }
        }
    }
}
