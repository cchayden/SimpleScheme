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

        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithOutputFile class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        private EvaluateCallWithOutputFile(object expr, Environment env, Stepper caller)
            : base(caller, expr, env)
        {
            ContinueHere(this.InitialStep);
            IncrementCounter(counter);
        }

        /// <summary>
        /// Gets the name of the stepper.
        /// </summary>
        public override string Name
        {
            get { return StepperName; }
        }

        /// <summary>
        /// Create an evaluator with output file.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        public static Stepper Call(object expr, Stepper caller)
        {
            return new EvaluateCallWithOutputFile(expr, caller.Env, caller);
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
        /// Open the output file and apply the proc.
        /// </summary>
        /// <returns>The next step in the application, or if the result is ready, 
        /// continues to the next step.</returns>
        private Stepper InitialStep()
        {
            this.port = OpenOutputFile(First(Expr));
            return Procedure.Proc(Second(Expr)).Apply(MakeList(this.port), ContinueHere(this.ReturnStep));
        }

        /// <summary>
        /// Closes the output port and returns the evaluation result.
        /// </summary>
        /// <returns>The evaluation result.</returns>
        private new Stepper ReturnStep()
        {
            if (this.port != null)
            {
                this.port.Close();
            }

            return ReturnFromStep(this.ReturnedExpr);
        }
    }
}
