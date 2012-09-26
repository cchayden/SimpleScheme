// <copyright file="EvaluateCallWithInputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-input-file expressions
    /// </summary>
    internal sealed class EvaluateCallWithInputFile : Evaluator
    {
        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper initialStep = GetStepper("InitialStep");

        /// <summary>
        /// Open instance method delegate
        /// </summary>
        private static readonly Stepper closeStep = GetStepper("CloseStep");

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
            : base(initialStep, args, env, caller, counter)
        {
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(port != null);
            Contract.Requires(counter >= 0);
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
            Contract.Requires(args != null);
            Contract.Requires(caller != null);
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
            Contract.Requires(filename != null);
            Contract.Requires(interp != null);
            try
            {
                return InputPort.New(new StreamReader(filename.ToString()), interp);
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
        /// <returns>The next evaluator, or else if the result is available, continue on to the next step.</returns>
        protected override Evaluator InitialStep()
        {
            var proc = (Procedure)Second(this.Expr);
            this.Pc = closeStep;
            return proc.Apply(MakeList(this.port), this, this);
        }

        /// <summary>
        /// Evaluation is complete: close the file and return the result.
        /// </summary>
        /// <returns>The evaluation result.</returns>
        protected override Evaluator CloseStep()
        {
            Contract.Assert(this.port != null);
            this.port.Close();

            Evaluator caller = this.Caller;
            caller.ReturnedExpr = this.ReturnedExpr;
            return caller;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.port != null);
        }
        #endregion
    }
}
