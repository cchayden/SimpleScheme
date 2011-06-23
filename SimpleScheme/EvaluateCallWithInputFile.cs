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

        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithInputFile class.
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        private EvaluateCallWithInputFile(Stepper caller, object expr, Environment env)
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
        /// Call a new evaluator with an input file
        /// </summary>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The created evaluator.</returns>
        public static Stepper Call(Stepper caller, object expr)
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
        /// Open the input file and apply the proc.  
        /// </summary>
        /// <returns>The next step, or else if the result is available, continue on to the next step.</returns>
        private Stepper InitialStep()
        {
            this.port = OpenInputFile(First(Expr));
            return Procedure.Proc(Second(Expr)).Apply(ContinueHere(this.ReturnStep), MakeList(this.port));
        }

        /// <summary>
        /// Evaluation is complete: close the file and return the result.
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
