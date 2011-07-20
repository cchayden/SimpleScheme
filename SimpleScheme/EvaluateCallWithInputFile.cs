// <copyright file="EvaluateCallWithInputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using Obj = System.Object;

    /// <summary>
    /// Evaluate a call-with-input-file expressions
    /// </summary>
    internal sealed class EvaluateCallWithInputFile : Stepper
    {
        #region Fields
        /// <summary>
        /// The name of the stepper, used for counters and tracing.
        /// </summary>
        private const string StepperName = "call-with-input-file";

        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create(StepperName);

        /// <summary>
        /// The input port to be used during the evaluation.
        /// </summary>
        private InputPort port;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithInputFile class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateCallWithInputFile(Obj expr, Environment env, Stepper caller)
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
        /// Call a new evaluator with an input file
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        internal static Stepper Call(Obj expr, Stepper caller)
        {
            return new EvaluateCallWithInputFile(expr, caller.Env, caller);
        }

        /// <summary>
        /// Open a file for input.
        /// </summary>
        /// <param name="filename">The filename of the file to open.</param>
        /// <returns>The input port, used for reading.</returns>
        internal static InputPort OpenInputFile(Obj filename)
        {
            try
            {
                return new InputPort(new StreamReader(Printer.AsString(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (InputPort)ErrorHandlers.IoError("No such file: " + Printer.AsString(filename));
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
        /// <returns>The next step, or else if the result is available, continue on to the next step.</returns>
        private Stepper InitialStep()
        {
            this.port = OpenInputFile(First(Expr));
            return Procedure.Proc(Second(Expr)).Apply(MakeList(this.port), this.Env, ContinueHere(this.CloseStep));
        }

        /// <summary>
        /// Evaluation is complete: close the file and return the result.
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
