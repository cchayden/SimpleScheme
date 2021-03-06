﻿// <copyright file="EvaluateCallWithOutputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-output-file expressions
    /// </summary>
    internal sealed class EvaluateCallWithOutputFile : Evaluator
    {
        #region Fields
        /// <summary>
        /// The output port to use during evaluation.
        /// </summary>
        private readonly OutputPort port;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithOutputFile class.
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="port">The output port.</param>
        private EvaluateCallWithOutputFile(SchemeObject args, Environment env, Evaluator caller, OutputPort port)
            : base(InitialStep, args, env, caller)
        {
            this.port = port;
        }
        #endregion

        #region Call
        /// <summary>
        /// Create an evaluator with output file.
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        internal static Evaluator Call(SchemeObject args, Evaluator caller)
        {
            OutputPort port = OpenOutputFile(First(args), caller.Interp);
            return new EvaluateCallWithOutputFile(args, caller.Env, caller, port);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Open a file for output.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The output port, used for writing.</returns>
        internal static OutputPort OpenOutputFile(SchemeObject filename, Interpreter interp)
        {
            try
            {
                return OutputPort.New(new StreamWriter(filename.ToString(false)), interp);
            }
            catch (FileNotFoundException)
            {
                ErrorHandlers.IoError("No such file: " + filename.ToString(true));
            }
            catch (IOException ex)
            {
                ErrorHandlers.IoError("IOException: " + ex.Message);
            }

            return null;
        }
        #endregion

        #region Steps
        /// <summary>
        /// Open the output file and apply the proc.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step in the application, or if the result is ready, 
        /// continues to the next step.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateCallWithOutputFile)s;
            var proc = Second(s.Expr);
            s.Pc = CloseStep;
            return ((Procedure)proc).Apply(MakeList(step.port), null, s, s);
        }

        /// <summary>
        /// Closes the output port and returns the evaluation result.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The evaluation result.</returns>
        private static Evaluator CloseStep(Evaluator s)
        {
            var step = (EvaluateCallWithOutputFile)s;
            if (step.port != null)
            {
                step.port.Close();
            }

            Evaluator caller = step.Caller;
            caller.ReturnedExpr = s.ReturnedExpr;
            return caller;
        }
        #endregion
    }
}
