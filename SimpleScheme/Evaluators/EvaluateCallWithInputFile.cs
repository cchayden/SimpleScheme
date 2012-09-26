// <copyright file="EvaluateCallWithInputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-input-file expressions
    /// </summary>
    public sealed class EvaluateCallWithInputFile : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("call-with-input-file");

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
            : base(args, env, caller, counter)
        {
            this.port = port;
            this.ContinueAt(InitialStep);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Call a new evaluator with an input file
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        public static Evaluator Call(SchemeObject args, Evaluator caller)
        {
            InputPort port = OpenInputFile(First(args), caller.Interp);
            return new EvaluateCallWithInputFile(args, caller.Env, caller, port);
        }

        /// <summary>
        /// Open a file for input.
        /// </summary>
        /// <param name="filename">The filename of the file to open.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The input port, used for reading.</returns>
        public static InputPort OpenInputFile(SchemeObject filename, Interpreter interp)
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

        #region Private Methods
        /// <summary>
        /// Open the input file and apply the proc.  
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next evaluator, or else if the result is available, continue on to the next step.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateCallWithInputFile)s;
            var proc = Second(s.Expr);
            return ((Procedure)proc).Apply(MakeList(step.port), s.ContinueAt(CloseStep));
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

            return step.ReturnFromStep(s.ReturnedExpr);
        }
        #endregion
    }
}
