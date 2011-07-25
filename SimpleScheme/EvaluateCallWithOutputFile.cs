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
    internal sealed class EvaluateCallWithOutputFile : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        internal const string StepperName = "evaluate-call-with-output-file";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The output port to use during evaluation.
        /// </summary>
        private OutputPort port;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithOutputFile class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateCallWithOutputFile(Obj expr, Environment env, Stepper caller)
            : base(expr, env, caller)
        {
            ContinueHere(InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Create an evaluator with output file.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        internal static Stepper Call(Obj expr, Stepper caller)
        {
            return new EvaluateCallWithOutputFile(expr, caller.Env, caller);
        }

        /// <summary>
        /// Open a file for output.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The output port, used for writing.</returns>
        internal static OutputPort OpenOutputFile(Obj filename, Interpreter interp)
        {
            try
            {
                return new OutputPort(new StreamWriter(Printer.AsString(filename, false)), interp);
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
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The next step in the application, or if the result is ready, 
        /// continues to the next step.</returns>
        private static Stepper InitialStep(Stepper s)
        {
            EvaluateCallWithOutputFile step = (EvaluateCallWithOutputFile)s;
            step.port = OpenOutputFile(List.First(s.Expr), s.Caller.Interp);
            return Procedure.Proc(List.Second(s.Expr)).Apply(List.New(step.port), s.ContinueHere(CloseStep));
        }

        /// <summary>
        /// Closes the output port and returns the evaluation result.
        /// </summary>
        /// <param name="s">The step to evaluate.</param>
        /// <returns>The evaluation result.</returns>
        private static Stepper CloseStep(Stepper s)
        {
            EvaluateCallWithOutputFile step = (EvaluateCallWithOutputFile)s;
            if (step.port != null)
            {
                step.port.Close();
            }

            return s.ReturnFromStep(s.ReturnedExpr);
        }
        #endregion
    }
}
