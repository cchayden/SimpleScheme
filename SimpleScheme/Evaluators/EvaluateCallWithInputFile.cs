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
        #region Fields
        /// <summary>
        /// The counter id.
        /// </summary>
        private static readonly int counter = Counter.Create("call-with-input-file");

        /// <summary>
        /// The input port to be used during the evaluation.
        /// </summary>
        private InputPort port;
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
            return New(args, caller.Env, caller, port);
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
                return new InputPort(new StreamReader(filename.ToString()), interp);
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
        /// <returns>The next step to execute.</returns>
        protected override Evaluator InitialStep()
        {
            var proc = (Procedure)Second(this.Expr);
            this.Pc = OpCode.Close;
            return proc.Apply(MakeList(this.port), this);
        }

        /// <summary>
        /// Evaluation is complete: close the file and return the result.
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
        /// Creates and initializes a new instance of the EvaluateCallWithInputFile class.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="port">The input port.</param>
        /// <returns>Initialized evaluator.</returns>
        private static EvaluateCallWithInputFile New(SchemeObject expr, Environment env, Evaluator caller, InputPort port)
        {
            Contract.Requires(expr != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            return new EvaluateCallWithInputFile().Initialize(expr, env, caller, port);
        }

        /// <summary>
        /// Initializes a new instance of the EvaluateCallWithInputFile class.
        /// </summary>
        /// <param name="args">A pair, containing a filename and a proc to evaluate.</param>
        /// <param name="env">The evaluation environment</param>
        /// <param name="caller">The caller.  Return to this when done.</param>
        /// <param name="p">The input port.</param>
        /// <returns>Initialized evaluator.</returns>
        private EvaluateCallWithInputFile Initialize(SchemeObject args, Environment env, Evaluator caller, InputPort p)
        {
            Contract.Requires(args != null);
            Contract.Requires(env != null);
            Contract.Requires(caller != null);
            Contract.Requires(p != null);
            Contract.Requires(counter >= 0);
            this.port = p;
            Initialize(OpCode.Initial, args, env, caller, counter);
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
