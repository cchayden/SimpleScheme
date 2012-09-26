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
        /// The output port to use during evaluation.
        /// </summary>
        private OutputPort port;
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
            return New(args, caller.Env, caller, port);
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
                return new OutputPort(new StreamWriter(filename.ToString()), interp);
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
        /// <returns>The next step to execute.</returns>
        protected override Evaluator InitialStep()
        {
            var proc = Second(this.Expr);
            this.Pc = OpCode.Close;
            return ((Procedure)proc).Apply(MakeList(this.port), this);
        }

        /// <summary>
        /// Closes the output port and returns the evaluation result.
        /// </summary>
        /// <returns>The next step to execute.</returns>
        protected override Evaluator CloseStep()
        {
            Contract.Assert(this.port != null);
            this.port.Close();
            return this.ReturnFromEvaluator(this.ReturnedExpr);
        }
        #endregion

        #region Initialize
        /// <summary>
        /// Creates and initializes a new instance of the EvaluateCallWithOutputFile class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="port">The output port.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateCallWithOutputFile New(SchemeObject expr, Environment env, Evaluator caller, OutputPort port)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return new EvaluateCallWithOutputFile().Initialize(expr, env, caller, port);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithOutputFile class.
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="p">The output port.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateCallWithOutputFile Initialize(SchemeObject args, Environment env, Evaluator caller, OutputPort p)
        {
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(p != null);
            this.port = p;
            Initialize(OpCode.Initial, args, env, caller);
            return this;
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.degenerate || this.port != null);
        }
        #endregion
    }
}
