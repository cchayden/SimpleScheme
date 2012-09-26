// <copyright file="EvaluateCallWithOutputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a call-with-output-file expressions
    /// </summary>
    public sealed class EvaluateCallWithOutputFile : Evaluator
    {
        #region Fields
        /// <summary>
        /// The name of the evaluator, used for counters and tracing.
        /// </summary>
        public const string EvaluatorName = "evaluate-call-with-output-file";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(EvaluatorName);

        /// <summary>
        /// The output port to use during evaluation.
        /// </summary>
        private readonly OutputPort port;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithOutputFile class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="port">The output port.</param>
        private EvaluateCallWithOutputFile(Obj expr, Environment env, Evaluator caller, OutputPort port)
            : base(expr, env, caller)
        {
            this.port = port;
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Create an evaluator with output file.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        public static Evaluator Call(Obj expr, Evaluator caller)
        {
            OutputPort port = OpenOutputFile(expr.First(), caller.Interp);
            return new EvaluateCallWithOutputFile(expr, caller.Env, caller, port);
        }

        /// <summary>
        /// Open a file for output.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The output port, used for writing.</returns>
        public static OutputPort OpenOutputFile(Obj filename, Interpreter interp)
        {
            try
            {
                return OutputPort.New(new StreamWriter(Printer.AsString(filename, false)), interp);
            }
            catch (FileNotFoundException)
            {
                ErrorHandlers.IoError("No such file: " + Printer.AsString(filename));
            }
            catch (IOException ex)
            {
                ErrorHandlers.IoError("IOException: " + ex.Message);
            }

            return null;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Open the output file and apply the proc.
        /// </summary>
        /// <param name="s">This evaluator.</param>
        /// <returns>The next step in the application, or if the result is ready, 
        /// continues to the next step.</returns>
        private static Evaluator InitialStep(Evaluator s)
        {
            var step = (EvaluateCallWithOutputFile)s;
            return s.Expr.Second().AsProcedure().Apply(step.port.MakeList(), s.ContinueHere(CloseStep));
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

            return s.ReturnFromStep(s.ReturnedExpr);
        }
        #endregion
    }
}
