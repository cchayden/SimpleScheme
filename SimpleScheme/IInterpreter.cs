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
        /// <summary>
        /// Gets the global environment of the interpreter.
        /// Each interpreter has its own global environment.
        /// The primitive environment is "below" this and is shared between interpreter instances.
        /// </summary>
        IEnvironment GlobalEnv { get; }

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

        /// <summary>
        /// Enter the Read-Eval-Print loop.
        /// </summary>
        /// <returns>The value of the last expression (before EOF).</returns>
        object ReadEvalPrintLoop();

        /// <summary>
        /// Load a file and evaluate the expressions in it.
        /// </summary>
        /// <param name="fileName">The file to load.</param>
        void LoadFile(object fileName);

        /// <summary>
        /// Read from the given input port and evaluate the expression.
        /// </summary>
        /// <param name="inp">The input port to read from.</param>
        /// <returns>The value of the evaluated expression.</returns>
        object ReadEval(InputPort inp);

        /// <summary>
        /// Load the program, contained in the given string, and execute it.
        /// </summary>
        /// <param name="str">The program to load.</param>
        void LoadString(string str);

        /// <summary>
        /// Evaluate the given string.
        /// </summary>
        /// <param name="str">The string to evaluate.</param>
        /// <returns>The value of the evaluated string.</returns>
        object EvalString(string str);
    }
}
