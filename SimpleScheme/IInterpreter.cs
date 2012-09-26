// <copyright file="IInterpreter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Interface for the SimpleScheme interpreter.
    /// </summary>
    public interface IInterpreter
    {
        #region Accessors
        /// <summary>
        /// Gets the global environment of the interpreter.
        /// Each interpreter has its own global environment.
        /// The primitive environment is "below" this and so it can shared between interpreter instances.
        /// </summary>
        IEnvironment GlobalEnv { get; }

        /// <summary>
        /// Gets the primitive environment for the interpreter.
        /// Each interpreter has one primitive environment.
        /// The primitive environment is the only one in which primitives can be defined.
        /// </summary>
        IPrimitiveEnvironment PrimEnv { get; }
        #endregion

        #region Setup Methods
        #endregion

        #region Read Methods
        /// <summary>
        /// Read an expression from the string and parse it into a Scheme object.
        /// </summary>
        /// <param name="str">The string to read.</param>
        /// <returns>The object that was read</returns>
        object Read(string str);

        /// <summary>
        /// Read a single expression from the input port.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The object that was read.</returns>
        object Read(InputPort inp);
        #endregion

        #region Evaluate Methods
        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        object Eval(object expr);

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="expr">The expression to evaluate, a s a string.</param>
        /// <returns>The result of the evaluation.</returns>
        /// <returns></returns>
        object EvalStr(string expr);

        /// <summary>
        /// Begin an asynchronous evaluation.  This may return before the evaluation is complete.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="cb">The callback.  This is called when the evaluation is complete.</param>
        /// <param name="state">State passed to the evaluator.</param>
        /// <returns>Async result, used to get the resulteval .</returns>
        IAsyncResult BeginEval(object expr, AsyncCallback cb, object state);

        /// <summary>
        /// Wrap up after an asynchronous evaluation.
        /// </summary>
        /// <param name="ar">The async results, used to get the evaluation result.</param>
        /// <returns>The evaluation result.</returns>
        object EndEval(IAsyncResult ar);
        #endregion

        #region Print Methods
        /// <summary>
        /// Return the scheme object as a string.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>A string representing the object value.</returns>
        string Print(object obj);
        #endregion

        #region Read/Eval Methods
        /// <summary>
        /// Read from the given input port and evaluate the expression.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The value of the evaluated expression.</returns>
        object ReadEval(InputPort inp);

        /// <summary>
        /// Load the program, contained in the given string, and execute it.
        /// This is similar to EvalStr, except that it handles any number of expressions.
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
        void LoadFile(object fileName);
    }
}
