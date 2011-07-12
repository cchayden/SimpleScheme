// <copyright file="InputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using Obj = System.Object;

    /// <summary>
    /// Represents an input port, a mechanism for reading input.
    /// </summary>
    public sealed class InputPort : ListPrimitives
    {
        #region Fields
        /// <summary>
        /// Marks the end of the input file.
        /// </summary>
        internal const string Eof = "#!EOF";
        #endregion

        #region Construcors
        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// </summary>
        /// <param name="inp">A text reader.</param>
        private InputPort(TextReader inp)
        {
            this.Parser = new Parser(inp);
        }
        #endregion

        #region Accessors

        /// <summary>
        /// Gets the parser.
        /// </summary>
        internal Parser Parser { get; private set; }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the input primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-input-from-file <string> <thunk>)</r4rs>
            //// <r4rs section="6.10.2">(char-ready?)</r4rs>
            //// <r4rs section="6.10.2">(char-ready? <port>)</r4rs>
            //// <r4rs section="6.10.4">(transcript-on <filename>)</r4rs>
            //// <r4rs section="6.10.4">(transcript-off)</r4rs>

            env
                //// <r4rs section="6.10.2">(eof-object? <obj>)</r4rs>
                .DefinePrimitive("eof-object?", (args, caller) => SchemeBoolean.Truth(IsEof(First(args))), 1)
                ////// <r4rs section="6.10.1">(call-with-input-file <string> <proc>)</r4rs>
                .DefinePrimitive("call-with-input-file", (args, caller) => EvaluateCallWithInputFile.Call(args, caller), 2)
                //// <r4rs section="6.10.1">(close-input-port <port>)</r4rs>
                .DefinePrimitive("close-input-port", (args, caller) => InPort(First(args), caller.Env.Interp.Input).Close(), 1)
                //// <r4rs section="6.10.1">(current-input-port)</r4rs>
                .DefinePrimitive("current-input-port", (args, caller) => caller.Env.Interp.Input, 0)
                //// <r4rs section="6.10.1">(input-port? <obj>)</r4rs>
                .DefinePrimitive("input-port?", (args, caller) => SchemeBoolean.Truth(First(args) is InputPort), 1)
                //// <r4rs section="6.10.4">(load <filename>)</r4rs>
                .DefinePrimitive("load", (args, caller) => LoadFile(caller, First(args)), 1)
                //// <r4rs section="6.10.1">(open-input-file <filename>)</r4rs>
                .DefinePrimitive("open-input-file", (args, caller) => EvaluateCallWithInputFile.OpenInputFile(First(args)), 1)
                //// <r4rs section="6.10.2">(peek-char)</r4rs>
                //// <r4rs section="6.10.2">(peek-char <port>)</r4rs>
                .DefinePrimitive("peek-char", (args, caller) => InPort(First(args), caller.Env.Interp.Input).Parser.PeekChar(), 0, 1)
                //// <r4rs section="6.10.2">(read)</r4rs>
                //// <r4rs section="6.10.2">(read <port>)</r4rs>
                .DefinePrimitive("read", (args, caller) => InPort(First(args), caller.Env.Interp.Input).Parser.Read(), 0, 1)
                //// <r4rs section="6.10.2">(read-char)</r4rs>
                //// <r4rs section="6.10.2">(read-char <port>)</r4rs>
                .DefinePrimitive("read-char", (args, caller) => InPort(First(args), caller.Env.Interp.Input).Parser.ReadChar(), 0, 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Creates a new InputPort.
        /// </summary>
        /// <param name="inp">A text reader</param>
        /// <returns>A new InputPort.</returns>
        internal static InputPort New(TextReader inp)
        {
            return new InputPort(inp);
        }

        /// <summary>
        /// Tests the obj against EOF.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>True if the obj is EOF.</returns>
        internal static bool IsEof(Obj x)
        {
            return x as string == Eof;
        }

        /// <summary>
        /// Check that an object is an input port.
        /// If the given obj is the empty list, return the interpreter's input port.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <param name="inPort">The default input port.</param>
        /// <returns>An input port.</returns>
        internal static InputPort InPort(Obj obj, InputPort inPort)
        {
            if (obj == List.Empty)
            {
                return inPort;
            }

            if (obj is InputPort)
            {
                return (InputPort)obj;
            }

            return InPort(ErrorHandlers.TypeError("input port", obj), null);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Read a complete scheme expression and parse it.
        /// Return the result.
        /// The result can be a list, or it could be a string (representing a symbol).
        /// </summary>
        /// <returns>The object that was read.</returns>
        internal Obj Read()
        {
            return this.Parser.Read();
        }

        /// <summary>
        /// Close the input port.
        /// Closes the stream used by the parser.
        /// </summary>
        /// <returns>The undefined instance.</returns>
        internal Obj Close()
        {
            this.Parser.Close();
            return Undefined.Instance;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Load a file given the filename.
        /// </summary>
        /// <param name="caller">The calling stepper.</param>
        /// <param name="filename">The file to load from.</param>
        /// <returns>The next step to execute.</returns>
        private static Obj LoadFile(Stepper caller, Obj filename)
        {
            caller.Env.Interp.LoadFile(filename);
            return Undefined.Instance;
        }
        #endregion
    }
}