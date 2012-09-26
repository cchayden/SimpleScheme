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
    /// It is immutable.
    /// </summary>
    public sealed class InputPort : Printable
    {
        #region Constants
        /// <summary>
        /// Marks the end of the input file.
        /// </summary>
        public const string Eof = "#!EOF";

        /// <summary>
        /// The printable name of the scheme input port type.
        /// </summary>
        public const string Name = "input-port";
        #endregion

        #region Fields
        /// <summary>
        /// This is used to parse the input.
        /// It holds state: a single character or a single token read-ahead, as well
        ///   as a transcript logger.
        /// </summary>
        private readonly Parser parser;

        /// <summary>
        /// The logger
        /// </summary>
        private readonly TranscriptLogger transcript;
        #endregion

        #region Construcor
        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// Store the TextReader and initialize the parser.
        /// </summary>
        /// <param name="inp">A text reader.</param>
        /// <param name="interp">The interpreter.</param>
        private InputPort(TextReader inp, Interpreter interp)
        {
            this.transcript = interp.Transcript;
            this.parser = new Parser(inp);
        }
        #endregion

        #region Internal Properties
        /// <summary>
        /// Gets the parser, for unit testing only.
        /// </summary>
        internal Parser Parser
        {
            get { return this.parser; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Creates a new instance of the InputPort class.
        /// Store the TextReader and initialize the parser.
        /// </summary>
        /// <param name="inp">A text reader.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>A new InputPort</returns>
        public static InputPort New(TextReader inp, Interpreter interp)
        {
            return new InputPort(inp, interp);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the input primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-input-from-file <string> <thunk>)</r4rs>
            //// <r4rs section="6.10.2">(char-ready?)</r4rs>
            //// <r4rs section="6.10.2">(char-ready? <port>)</r4rs>

            env
                //// <r4rs section="6.10.2">(eof-object? <obj>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("eof-object?"), 
                        (args, caller) => SchemeBoolean.Truth(IsEof(args.First())), 
                        1, 
                        Primitive.ValueType.Obj)
                ////// <r4rs section="6.10.1">(call-with-input-file <string> <proc>)</r4rs>
                .DefinePrimitive(
                       Symbol.New("call-with-input-file"), 
                       (args, caller) => EvaluateCallWithInputFile.Call(args, caller), 
                       2, 
                       Primitive.ValueType.String, 
                       Primitive.ValueType.Proc)
                //// <r4rs section="6.10.1">(close-input-port <port>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("close-input-port"), 
                        (args, caller) => Port(args.First(), caller.Interp.CurrentInputPort).CloseInputPort(), 
                        1, 
                        Primitive.ValueType.Port)
                //// <r4rs section="6.10.1">(current-input-port)</r4rs>
                .DefinePrimitive(
                        Symbol.New("current-input-port"), 
                        (args, caller) => caller.Interp.CurrentInputPort, 
                        0)
                //// <r4rs section="6.10.1">(input-port? <obj>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("input-port?"), 
                        (args, caller) => SchemeBoolean.Truth(args.First().IsInputPort()), 
                        1, 
                        Primitive.ValueType.Obj)
                //// <r4rs section="6.10.4">(load <filename>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("load"), 
                        (args, caller) => LoadFile(args.First(), caller.Interp), 
                        1, 
                        Primitive.ValueType.String)
                //// <r4rs section="6.10.1">(open-input-file <filename>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("open-input-file"), 
                        (args, caller) => EvaluateCallWithInputFile.OpenInputFile(args.First(), caller.Interp), 
                        1,
                        Primitive.ValueType.String)
                //// <r4rs section="6.10.2">(peek-char)</r4rs>
                //// <r4rs section="6.10.2">(peek-char <port>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("peek-char"), 
                        (args, caller) => Port(args.First(), caller.Interp.CurrentInputPort).PeekChar(), 
                        0, 
                        1, 
                        Primitive.ValueType.Port)
                //// <r4rs section="6.10.2">(read)</r4rs>
                //// <r4rs section="6.10.2">(read <port>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("read"), 
                        (args, caller) => Port(args.First(), caller.Interp.CurrentInputPort).Read(), 
                        0, 
                        1, 
                        Primitive.ValueType.Port)
                //// <r4rs section="6.10.2">(read-char)</r4rs>
                //// <r4rs section="6.10.2">(read-char <port>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("read-char"), 
                        (args, caller) => Port(args.First(), caller.Interp.CurrentInputPort).ReadChar(), 
                        0, 
                        1, 
                        Primitive.ValueType.Port)
                //// <r4rs section="6.10.4">(transcript-on <filename>)</r4rs>
                .DefinePrimitive(
                        Symbol.New("transcript-on"), 
                        (args, caller) => TranscriptOn(args.First(), caller.Interp.Transcript), 
                        1, 
                        Primitive.ValueType.String)
                //// <r4rs section="6.10.4">(transcript-off)</r4rs>
                .DefinePrimitive(
                        Symbol.New("transcript-off"), 
                        (args, caller) => TranscriptOff(caller.Interp.Transcript), 
                        0);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests the obj against EOF.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>True if the obj is EOF.</returns>
        public static bool IsEof(Obj x)
        {
            return x as string == Eof;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the input port to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Read an expression from the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <returns>The expression read.</returns>
        public Obj Read()
        {
            var sb = new StringBuilder();
            Obj expr = this.parser.ReadExpr(sb);
            this.transcript.LogInputLine(sb.ToString().Trim(), this);
            return expr;
        }

        /// <summary>
        /// Close the input port.
        /// Closes the stream used by the parser.
        /// </summary>
        public void Close()
        {
            this.parser.Close();
        }

        /// <summary>
        /// Display the input port as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The input port type name.</returns>
        public override string ToString()
        {
            return "<" + Name + ">";
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Determine the port object to use with the InputPort primitives.
        /// The port is optional: if supplied, it is the port to use.
        /// Otherwise, the current input port is used instead.
        /// </summary>
        /// <param name="port">The port to use, if supplied.</param>
        /// <param name="curr">The current input port.</param>
        /// <returns>The port to use.</returns>
        private static InputPort Port(Obj port, InputPort curr)
        {
            return port.IsEmptyList() ? curr : port.AsInputPort();
        }

        /// <summary>
        /// Load a file given the filename.
        /// </summary>
        /// <param name="filename">The file to load from.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>Undefined object.</returns>
        private static Obj LoadFile(Obj filename, Interpreter interp)
        {
            interp.LoadFile(filename);
            return Undefined.New();
        }

        /// <summary>
        /// Turn the transcript on.
        /// </summary>
        /// <param name="filename">The file to write to.</param>
        /// <param name="transcript">The transcript logger.</param>
        /// <returns>Undefined object.</returns>
        private static Obj TranscriptOn(Obj filename, TranscriptLogger transcript)
        {
            transcript.TranscriptOn(filename);
            return Undefined.New();
        }

        /// <summary>
        /// Turn the transcript off.
        /// </summary>
        /// <param name="transcript">The transcript logger.</param>
        /// <returns>Undefined object.</returns>
        private static Obj TranscriptOff(TranscriptLogger transcript)
        {
            transcript.TranscriptOff();
            return Undefined.New();
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Close the input port.
        /// </summary>
        /// <returns>Undefined obect.</returns>
        private Obj CloseInputPort()
        {
            this.Close();
            return Undefined.New();
        }

        /// <summary>
        /// Peek for a character on the input port.
        /// </summary>
        /// <returns>The character in the input.</returns>
        private Obj PeekChar()
        {
            return this.parser.PeekChar();
        }

        /// <summary>
        /// Read a character on the input port.
        /// </summary>
        /// <returns>The character read.</returns>
        private Obj ReadChar()
        {
            var sb = new StringBuilder();
            Obj expr = this.parser.ReadChar(sb);
            this.transcript.LogInputLine(sb.ToString().Trim(), this);
            return expr;
        }
        #endregion
    }

    /// <summary>
    /// Extensions for InputPort
    /// </summary>
    public static class InputPortExtensions
    {
        /// <summary>
        /// Tests whether to given object is a scheme input port.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme input port.</returns>
        public static bool IsInputPort(this Obj obj)
        {
            return obj is InputPort;
        }

        /// <summary>
        /// Check that an object is an input port.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>An input port.</returns>
        public static InputPort AsInputPort(this Obj obj)
        {
            if (obj.IsInputPort())
            {
                return (InputPort)obj;
            }

            ErrorHandlers.TypeError(InputPort.Name, obj);
            return null;
        }
    }
}