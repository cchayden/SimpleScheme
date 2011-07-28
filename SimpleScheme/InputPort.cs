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
    public sealed class InputPort
    {
        #region Constants
        /// <summary>
        /// Marks the end of the input file.
        /// </summary>
        internal const string Eof = "#!EOF";

        /// <summary>
        /// The printable name of the scheme input port type.
        /// </summary>
        private const string Name = "input port";
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
        internal InputPort(TextReader inp, Interpreter interp)
        {
            this.transcript = interp.Transcript;
            this.parser = new Parser(inp);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme input port.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme input port.</returns>
        public static bool IsInputPort(Obj obj)
        {
            return obj is InputPort;
        }
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

            env
                //// <r4rs section="6.10.2">(eof-object? <obj>)</r4rs>
                .DefinePrimitive("eof-object?", (args, caller) => SchemeBoolean.Truth(IsEof(List.First(args))), 1)
                ////// <r4rs section="6.10.1">(call-with-input-file <string> <proc>)</r4rs>
                .DefinePrimitive("call-with-input-file", (args, caller) => EvaluateCallWithInputFile.Call(args, caller), 2)
                //// <r4rs section="6.10.1">(close-input-port <port>)</r4rs>
                .DefinePrimitive("close-input-port", (args, caller) => CloseInputPort(List.First(args), caller.Interp), 1)
                //// <r4rs section="6.10.1">(current-input-port)</r4rs>
                .DefinePrimitive("current-input-port", (args, caller) => caller.Interp.CurrentInputPort, 0)
                //// <r4rs section="6.10.1">(input-port? <obj>)</r4rs>
                .DefinePrimitive("input-port?", (args, caller) => SchemeBoolean.Truth(IsInputPort(List.First(args))), 1)
                //// <r4rs section="6.10.4">(load <filename>)</r4rs>
                .DefinePrimitive("load", (args, caller) => LoadFile(List.First(args), caller.Interp), 1)
                //// <r4rs section="6.10.1">(open-input-file <filename>)</r4rs>
                .DefinePrimitive("open-input-file", (args, caller) => EvaluateCallWithInputFile.OpenInputFile(List.First(args), caller.Interp), 1)
                //// <r4rs section="6.10.2">(peek-char)</r4rs>
                //// <r4rs section="6.10.2">(peek-char <port>)</r4rs>
                .DefinePrimitive("peek-char", (args, caller) => PeekChar(List.First(args), caller.Interp), 0, 1)
                //// <r4rs section="6.10.2">(read)</r4rs>
                //// <r4rs section="6.10.2">(read <port>)</r4rs>
                .DefinePrimitive("read", (args, caller) => Read(List.First(args), caller.Interp), 0, 1)
                //// <r4rs section="6.10.2">(read-char)</r4rs>
                //// <r4rs section="6.10.2">(read-char <port>)</r4rs>
                .DefinePrimitive("read-char", (args, caller) => ReadChar(List.First(args), caller.Interp), 0, 1)
                //// <r4rs section="6.10.4">(transcript-on <filename>)</r4rs>
                .DefinePrimitive("transcript-on", (args, caller) => TranscriptOn(List.First(args), caller.Interp), 1)
                //// <r4rs section="6.10.4">(transcript-off)</r4rs>
                .DefinePrimitive("transcript-off", (args, caller) => TranscriptOff(caller.Interp), 0);
        }
        #endregion

        #region Static Methods
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
        internal static InputPort AsInputPort(Obj obj)
        {
            if (IsInputPort(obj))
            {
                return (InputPort)obj;
            }

            ErrorHandlers.TypeError(Name, obj);
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
            StringBuilder sb = new StringBuilder();
            Obj expr = this.parser.ReadExpr(sb);
            this.transcript.LogInputLine(sb.ToString().Trim(), this);
            return expr;
        }

        /// <summary>
        /// Close the input port.
        /// Closes the stream used by the parser.
        /// </summary>
        internal void Close()
        {
            this.parser.Close();
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Determine the port object to use with the InputPort primitives.
        /// The port is optional: if supplied, it is the port to use.
        /// Otherwise, the current input port is used instead.
        /// </summary>
        /// <param name="port">The port to use, if supplied.</param>
        /// <param name="interp">The interpreter, from which the current input port can be obtained.</param>
        /// <returns>The port to use.</returns>
        private static InputPort Port(Obj port, Interpreter interp)
        {
            return EmptyList.IsEmptyList(port) ? interp.CurrentInputPort : AsInputPort(port);
        }

        /// <summary>
        /// Load a file given the filename.
        /// </summary>
        /// <param name="filename">The file to load from.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>Undefined object.</returns>
        private static object LoadFile(Obj filename, Interpreter interp)
        {
            interp.LoadFile(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript on.
        /// </summary>
        /// <param name="filename">The file to write to.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>Undefined object.</returns>
        private static object TranscriptOn(Obj filename, Interpreter interp)
        {
            interp.Transcript.TranscriptOn(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript off.
        /// </summary>
        /// <param name="interp">The interpreter.</param>
        /// <returns>Undefined object.</returns>
        private static object TranscriptOff(Interpreter interp)
        {
            interp.Transcript.TranscriptOff();
            return Undefined.Instance;
        }

        /// <summary>
        /// Close the given input port.
        /// If none given, closes the default input port.
        /// </summary>
        /// <param name="port">The port to close.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>Undefined obect.</returns>
        private static Obj CloseInputPort(Obj port, Interpreter interp)
        {
            Port(port, interp).Close();
            return Undefined.Instance;
        }

        /// <summary>
        /// Peek for a character on the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <param name="port">The port to use.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The character in the input.</returns>
        private static Obj PeekChar(Obj port, Interpreter interp)
        {
            InputPort p = Port(port, interp);
            return p.parser.PeekChar();
        }

        /// <summary>
        /// Read an expression from the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <param name="port">The port to use.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The expression read.</returns>
        private static Obj Read(Obj port, Interpreter interp)
        {
            StringBuilder sb = new StringBuilder();
            InputPort p = Port(port, interp);
            Obj expr = p.parser.ReadExpr(sb);
            interp.Transcript.LogInputLine(sb.ToString().Trim(), p);
            return expr;
        }

        /// <summary>
        /// Read a character on the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <param name="port">The port to use.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>The character read.</returns>
        private static Obj ReadChar(Obj port, Interpreter interp)
        {
            StringBuilder sb = new StringBuilder();
            InputPort p = Port(port, interp);
            Obj expr = p.parser.ReadChar(sb);
            interp.Transcript.LogInputLine(sb.ToString().Trim(), p);
            return expr;
        }
        #endregion
    }

    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    internal static partial class Extensions
    {
        /// <summary>
        /// Write the input port to the string builder.
        /// </summary>
        /// <param name="port">The input port (not used).</param>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        internal static void AsString(this InputPort port, bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append("<input port>");
            }
        }
    }
}