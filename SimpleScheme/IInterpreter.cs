// <copyright file="IInterpreter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Interface for the SimpleScheme interpreter.
    /// </summary>
    [ContractClass(typeof(IInterpreterContract))]
    public interface IInterpreter
    {
        #region Accessors
        /// <summary>
        /// Gets the global environment of the interpreter.
        /// Each interpreter has its own global environment.
        /// The primitive environment is "below" this and so it can shared between interpreter instances.
        /// </summary>
        IEnvironment GlobalEnvironment { get; }

        /// <summary>
        /// Gets the primitive environment for the interpreter.
        /// Each interpreter has one primitive environment.
        /// The primitive environment is the only one in which primitives can be defined.
        /// </summary>
        IPrimitiveEnvironment PrimEnvironment { get; }
        #endregion

        #region Setup Methods
        #endregion

        #region Read Methods
        /// <summary>
        /// Read an expression from the string and parse it into a Scheme object.
        /// </summary>
        /// <param name="str">The string to read.</param>
        /// <returns>The object that was read</returns>
        SchemeObject Read(string str);

        /// <summary>
        /// Read a single expression from the input port.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The object that was read.</returns>
        SchemeObject Read(InputPort inp);
        #endregion

        #region Evaluate Methods
        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        SchemeObject Eval(SchemeObject expr);

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="expr">The expression to evaluate, a s a string.</param>
        /// <returns>The result of the evaluation.</returns>
        /// <returns></returns>
        SchemeObject Eval(string expr);

        /// <summary>
        /// Begin an asynchronous evaluation.  This may return before the evaluation is complete.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="cb">The callback.  This is called when the evaluation is complete.</param>
        /// <param name="state">State passed to the evaluator.</param>
        /// <returns>Async result, used to get the resulteval .</returns>
        IAsyncResult BeginEval(SchemeObject expr, AsyncCallback cb, object state);

        /// <summary>
        /// Wrap up after an asynchronous evaluation.
        /// </summary>
        /// <param name="ar">The async results, used to get the evaluation result.</param>
        /// <returns>The evaluation result.</returns>
        SchemeObject EndEval(IAsyncResult ar);
        #endregion

        #region Print Methods
        /// <summary>
        /// Return the scheme object as a string.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>A string representing the object value.</returns>
        string Print(SchemeObject obj);
        #endregion

        #region Read/Eval Methods
        /// <summary>
        /// Read from the given input port and evaluate the expression.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The value of the evaluated expression.</returns>
        SchemeObject ReadEval(InputPort inp);

        /// <summary>
        /// Load the program, contained in the given string, and execute it.
        /// This is similar to Eval, except that it handles any number of expressions.
        /// </summary>
        /// <param name="str">The program to load.</param>
        void Load(string str);
        #endregion

        #region Read/Eval/Print Methods
        /// <summary>
        /// Read, evaluate, print, and return the result.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The evaluated result, printed as a string.</returns>
        string ReadEvalPrint(InputPort inp);

        /// <summary>
        /// Read, evaluate, print, and return the result.
        /// </summary>
        /// <param name="str">The string to read from.</param>
        /// <returns>The evaluated result, printed as a string.</returns>
        string ReadEvalPrint(string str);

        /// <summary>
        /// Enter the Read-Eval-Print loop.
        /// </summary>
        void ReadEvalPrintLoop();

        /// <summary>
        /// Read and evaluate an expression, using the async evaluator.
        /// </summary>
        /// <returns>If end of file, InputPort.Eof, otherwise the IAsyncResult.</returns>
        IAsyncResult ReadEvalPrintAsync();
        #endregion

        /// <summary>
        /// Load a file and evaluate the expressions in it.
        /// </summary>
        /// <param name="fileName">The file to load.</param>
        /// <param name="outp">Input and results are written to this port, if not null.</param>
        void LoadFile(SchemeObject fileName, OutputPort outp);
    }

    /// <summary>
    /// Define the contract for IInterpreter
    /// </summary>    
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1611:ElementParametersMustBeDocumented", Justification = "Contract.")]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1615:ElementReturnValueMustBeDocumented", Justification = "Contract.")]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1600:ElementsMustBeDocumented", Justification = "Contract.")]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1600:ElementsMustBeDocumented", Justification = "Reviewed. Suppression is OK here.")]
    [ContractClassFor(typeof(IInterpreter))]
    internal abstract class IInterpreterContract : IInterpreter
    {
        public IEnvironment GlobalEnvironment
        {
            get
            {
                Contract.Ensures(Contract.Result<IEnvironment>() != null);
                return null;
            }
        }

        public IPrimitiveEnvironment PrimEnvironment
        {
            get
            {
                Contract.Ensures(Contract.Result<IPrimitiveEnvironment>() != null);
                return null;
            }
        }

        public SchemeObject Read(string str)
        {
            Contract.Requires(str != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            return null;
        }

        public SchemeObject Read(InputPort inp)
        {
            Contract.Requires(inp != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            return null;
        }

        public SchemeObject Eval(SchemeObject expr)
        {
            Contract.Requires(expr != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            return null;
        }

        public SchemeObject Eval(string expr)
        {
            Contract.Requires(expr != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            return null;
        }

        public IAsyncResult BeginEval(SchemeObject expr, AsyncCallback cb, object state)
        {
            Contract.Requires(expr != null);
            return null;
        }

        public SchemeObject EndEval(IAsyncResult ar)
        {
            Contract.Requires(ar != null);
            return null;
        }

        public string Print(SchemeObject obj)
        {
            Contract.Requires(obj != null);
            Contract.Ensures(Contract.Result<string>() != null);
            return null;
        }

        public SchemeObject ReadEval(InputPort inp)
        {
            Contract.Requires(inp != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            return null;
        }

        public void Load(string str)
        {
            Contract.Requires(str != null);
        }

        public string ReadEvalPrint(InputPort inp)
        {
            Contract.Requires(inp != null);
            Contract.Ensures(Contract.Result<string>() != null);
            return null;
        }

        public string ReadEvalPrint(string str)
        {
            Contract.Requires(str != null);
            Contract.Ensures(Contract.Result<string>() != null);
            return null;
        }

        public void ReadEvalPrintLoop()
        {
        }

        public IAsyncResult ReadEvalPrintAsync()
        {
            return null;
        }

        public void LoadFile(SchemeObject fileName, OutputPort outp)
        {
            Contract.Requires(fileName != null);
        }
    }
}
