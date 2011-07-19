// <copyright file="InputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;
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
        internal static void DefinePrimitives(PrimitiveEnvironment env)
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
                .DefinePrimitive("close-input-port", (args, caller) => CloseInputPort(First(args), caller), 1)
                //// <r4rs section="6.10.1">(current-input-port)</r4rs>
                .DefinePrimitive("current-input-port", (args, caller) => caller.CurrentInputPort, 0)
                //// <r4rs section="6.10.1">(input-port? <obj>)</r4rs>
                .DefinePrimitive("input-port?", (args, caller) => SchemeBoolean.Truth(TypePrimitives.IsInputPort(First(args))), 1)
                //// <r4rs section="6.10.4">(load <filename>)</r4rs>
                .DefinePrimitive("load", (args, caller) => LoadFile(First(args), caller), 1)
                //// <r4rs section="6.10.1">(open-input-file <filename>)</r4rs>
                .DefinePrimitive("open-input-file", (args, caller) => EvaluateCallWithInputFile.OpenInputFile(First(args)), 1)
                //// <r4rs section="6.10.2">(peek-char)</r4rs>
                //// <r4rs section="6.10.2">(peek-char <port>)</r4rs>
                .DefinePrimitive("peek-char", (args, caller) => PeekChar(First(args), caller), 0, 1)
                //// <r4rs section="6.10.2">(read)</r4rs>
                //// <r4rs section="6.10.2">(read <port>)</r4rs>
                .DefinePrimitive("read", (args, caller) => Read(First(args), caller), 0, 1)
                //// <r4rs section="6.10.2">(read-char)</r4rs>
                //// <r4rs section="6.10.2">(read-char <port>)</r4rs>
                .DefinePrimitive("read-char", (args, caller) => ReadChar(First(args), caller), 0, 1);
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
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>An input port.</returns>
        internal static InputPort InPort(Obj obj)
        {
            if (TypePrimitives.IsInputPort(obj))
            {
                return (InputPort)obj;
            }

            ErrorHandlers.TypeError(TypePrimitives.InputPortName, obj);
            return null;
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Read a complete scheme expression and parse it.
        /// Return the result.
        /// The result can be a list, or it could be a string (representing a symbol).
        /// </summary>
        /// <returns>The object that was read.</returns>
        internal Obj ReadObj()
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
        /// <param name="filename">The file to load from.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The next step to execute.</returns>
        private static object LoadFile(object filename, Stepper caller)
        {
            caller.LoadFile(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Close the given input port.
        /// If none given, closes the default input port.
        /// </summary>
        /// <param name="port">The port to close.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>Undefined obect.</returns>
        private static Obj CloseInputPort(Obj port, Stepper caller)
        {
            InputPort p = TypePrimitives.IsEmptyList(port) ? caller.CurrentInputPort : InPort(port);
            return p.Close();
        }

        /// <summary>
        /// Peek for a character on the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <param name="port">The port to use.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The character in the input.</returns>
        private static Obj PeekChar(Obj port, Stepper caller)
        {
            InputPort p = TypePrimitives.IsEmptyList(port) ? caller.CurrentInputPort : InPort(port);
            return p.Parser.PeekChar();
        }

        /// <summary>
        /// Read an expression from the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <param name="port">The port to use.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The expression read.</returns>
        private static Obj Read(Obj port, Stepper caller)
        {
            InputPort p = TypePrimitives.IsEmptyList(port) ? caller.CurrentInputPort : InPort(port);
            return p.Parser.Read();
        }

        /// <summary>
        /// Read a character on the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <param name="port">The port to use.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The character read.</returns>
        private static Obj ReadChar(Obj port, Stepper caller)
        {
            InputPort p = TypePrimitives.IsEmptyList(port) ? caller.CurrentInputPort : InPort(port);
            return p.Parser.ReadChar();
        }
        #endregion
    }
}