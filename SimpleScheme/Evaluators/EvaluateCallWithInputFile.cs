// <copyright file="EvaluateCallWithInputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-input-file expressions
    /// </summary>
    internal sealed class EvaluateCallWithInputFile : Evaluator
    {
        #region Fields
        /// <summary>
        /// The input port to be used during the evaluation.
        /// </summary>
        private readonly InputPort port;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithInputFile class.
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="port">The input port.</param>
        private EvaluateCallWithInputFile(SchemeObject args, Environment env, Evaluator caller, InputPort port)
            : base(InitialStep, args, env, caller)
        {
            this.port = port;
        }
        #endregion

        #region Call
        /// <summary>
        /// Call a new evaluator with an input file
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        internal static Evaluator Call(SchemeObject args, Evaluator caller)
        {
            InputPort port = OpenInputFile(First(args), caller.Interp);
            return new EvaluateCallWithInputFile(args, caller.Env, caller, port);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Open a file for input.
        /// </summary>
        /// <param name="filename">The filename of the file to open.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The input port, used for reading.</returns>
        internal static InputPort OpenInputFile(SchemeObject filename, Interpreter interp)
        {
            try
            {
                return InputPort.New(new StreamReader(filename.ToString(false)), interp);
            }
            catch (FileNotFoundException)
            {
                return (InputPort)ErrorHandlers.IoError("No such file: " + filename.ToString(true));
            }
            catch (IOException ex)
            {
                return (InputPort)ErrorHandlers.IoError("IOException: " + ex.Message);
            }
        }
        #endregion

        #region Steps
        /// <summary>
        /// Open the input file and apply the proc.  
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator, or else if the result is available, continue on to the next step.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateCallWithInputFile)s;
            var proc = (Procedure)Second(s.Expr);
            s.Pc = CloseStep;
            return proc.Apply(MakeList(step.port), null, s, s);
        }

        /// <summary>
        /// Evaluation is complete: close the file and return the result.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The evaluation result.</returns>
        private static Evaluator CloseStep(Evaluator s)
        {
            var step = (EvaluateCallWithInputFile)s;
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
