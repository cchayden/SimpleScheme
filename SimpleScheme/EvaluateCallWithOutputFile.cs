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
        private const string StepperName = "evaluate-call-with-output-file";

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
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        internal override string Name
        {
            get { return StepperName; }
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
        /// <returns>The next step in the application, or if the result is ready, 
        /// continues to the next step.</returns>
        private Stepper InitialStep()
        {
            this.port = OpenOutputFile(List.First(Expr), Caller.Interp);
            return Procedure.Proc(List.Second(Expr)).Apply(List.New(this.port), this.Env, ContinueHere(this.CloseStep));
        }

        /// <summary>
        /// Closes the output port and returns the evaluation result.
        /// </summary>
        /// <returns>The evaluation result.</returns>
        private Stepper CloseStep()
        {
            if (this.port != null)
            {
                this.port.Close();
            }

            return ReturnFromStep(this.ReturnedExpr);
        }
        #endregion
    }
}
