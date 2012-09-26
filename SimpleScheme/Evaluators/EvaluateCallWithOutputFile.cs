// <copyright file="EvaluateCallWithOutputFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.Contracts;
    using System.IO;

    /// <summary>
    /// Evaluate a call-with-output-file expressions
    /// </summary>
    internal sealed class EvaluateCallWithOutputFile : Evaluator
    {
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("evaluate-call-with-output-file");

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
            : base(OpCode.Initial, args, env, caller, counter)
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
        /// Create an evaluator with output file.
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <returns>The created evaluator.</returns>
        internal static Evaluator Call(SchemeObject args, Evaluator caller)
        {
            Contract.Requires(args != null);
            Contract.Requires(caller != null);
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
            Contract.Requires(filename != null);
            Contract.Requires(interp != null);
            try
            {
                return OutputPort.New(new StreamWriter(filename.ToString()), interp);
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
        /// <returns>The next step in the application, or if the result is ready, 
        /// continues to the next step.</returns>
        protected override Evaluator InitialStep()
        {
            var proc = Second(this.Expr);
            this.Pc = OpCode.Close;
            return ((Procedure)proc).Apply(MakeList(this.port), this, this);
        }

        /// <summary>
        /// Closes the output port and returns the evaluation result.
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
