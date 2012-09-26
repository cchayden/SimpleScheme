// <copyright file="InputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;

    /// <summary>
    /// Represents an input port, a mechanism for reading input.
    /// It is immutable.
    /// </summary>
    public sealed class InputPort : IPrintable, ISchemeObject
    {
        #region Constants

        /// <summary>
        /// Marks the end of the input file.
        /// </summary>
        public static readonly Eof Eof = Eof.Instance;
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
        public InputPort(TextReader inp, Interpreter interp)
        {
            this.transcript = interp.Transcript;
            this.parser = new Parser(inp);
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Port); }
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

        #region New
        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// Store the TextReader and initialize the parser.
        /// </summary>
        /// <param name="inp">A text reader.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>An input port.</returns>
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
                        "eof-object?",
                        (args, caller) => SchemeBoolean.Truth(List.First(args) is Eof), 
                        1, 
                        TypePrimitives.ValueType.Obj)
                ////// <r4rs section="6.10.1">(call-with-input-file <string> <proc>)</r4rs>
                .DefinePrimitive(
                       "call-with-input-file",
                       EvaluateCallWithInputFile.Call, 
                       2, 
                       TypePrimitives.ValueType.String, 
                       TypePrimitives.ValueType.Proc)
                //// <r4rs section="6.10.1">(close-input-port <port>)</r4rs>
                .DefinePrimitive(
                        "close-input-port",
                        (args, caller) => Port(List.First(args), caller.Interp.CurrentInputPort).CloseInputPort(), 
                        1, 
                        TypePrimitives.ValueType.Port)
                //// <r4rs section="6.10.1">(current-input-port)</r4rs>
                .DefinePrimitive(
                        "current-input-port",
                        (args, caller) => caller.Interp.CurrentInputPort, 
                        0)
                //// <r4rs section="6.10.1">(input-port? <obj>)</r4rs>
                .DefinePrimitive(
                        "input-port?",
                        (args, caller) => SchemeBoolean.Truth(List.First(args) is InputPort), 
                        1, 
                        TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.10.4">(load <filename>)</r4rs>
                .DefinePrimitive(
                        "load",
                        (args, caller) => LoadFile(List.First(args), caller.Interp), 
                        1, 
                        TypePrimitives.ValueType.String)
                //// <r4rs section="6.10.1">(open-input-file <filename>)</r4rs>
                .DefinePrimitive(
                        "open-input-file",
                        (args, caller) => EvaluateCallWithInputFile.OpenInputFile(List.First(args), caller.Interp), 
                        1,
                        TypePrimitives.ValueType.String)
                //// <r4rs section="6.10.2">(peek-char)</r4rs>
                //// <r4rs section="6.10.2">(peek-char <port>)</r4rs>
                .DefinePrimitive(
                        "peek-char",
                        (args, caller) => Port(List.First(args), caller.Interp.CurrentInputPort).PeekChar(), 
                        0, 
                        1, 
                        TypePrimitives.ValueType.Port)
                //// <r4rs section="6.10.2">(read)</r4rs>
                //// <r4rs section="6.10.2">(read <port>)</r4rs>
                .DefinePrimitive(
                        "read",
                        (args, caller) => Port(List.First(args), caller.Interp.CurrentInputPort).Read(), 
                        0, 
                        1, 
                        TypePrimitives.ValueType.Port)
                //// <r4rs section="6.10.2">(read-char)</r4rs>
                //// <r4rs section="6.10.2">(read-char <port>)</r4rs>
                .DefinePrimitive(
                        "read-char",
                        (args, caller) => Port(List.First(args), caller.Interp.CurrentInputPort).ReadChar(), 
                        0, 
                        1, 
                        TypePrimitives.ValueType.Port)
                //// <r4rs section="6.10.4">(transcript-on <filename>)</r4rs>
                .DefinePrimitive(
                        "transcript-on",
                        (args, caller) => TranscriptOn(List.First(args), caller.Interp.Transcript), 
                        1, 
                        TypePrimitives.ValueType.String)
                //// <r4rs section="6.10.4">(transcript-off)</r4rs>
                .DefinePrimitive(
                        "transcript-off",
                        (args, caller) => TranscriptOff(caller.Interp.Transcript), 
                        0);
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
        public ISchemeObject Read()
        {
            var sb = new StringBuilder();
            ISchemeObject expr = this.parser.ReadExpr(sb);
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
            return "<input-port>";
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
        private static InputPort Port(ISchemeObject port, InputPort curr)
        {
            return port is EmptyList ? curr : port.AsInputPort();
        }

        /// <summary>
        /// Load a file given the filename.
        /// </summary>
        /// <param name="filename">The file to load from.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>Undefined object.</returns>
        private static ISchemeObject LoadFile(ISchemeObject filename, Interpreter interp)
        {
            interp.LoadFile(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript on.
        /// </summary>
        /// <param name="filename">The file to write to.</param>
        /// <param name="transcript">The transcript logger.</param>
        /// <returns>Undefined object.</returns>
        private static ISchemeObject TranscriptOn(ISchemeObject filename, TranscriptLogger transcript)
        {
            transcript.TranscriptOn(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript off.
        /// </summary>
        /// <param name="transcript">The transcript logger.</param>
        /// <returns>Undefined object.</returns>
        private static ISchemeObject TranscriptOff(TranscriptLogger transcript)
        {
            transcript.TranscriptOff();
            return Undefined.Instance;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Close the input port.
        /// </summary>
        /// <returns>Undefined obect.</returns>
        private ISchemeObject CloseInputPort()
        {
            this.Close();
            return Undefined.Instance;
        }

        /// <summary>
        /// Peek for a character on the input port.
        /// </summary>
        /// <returns>The character in the input.</returns>
        private ISchemeObject PeekChar()
        {
            return this.parser.PeekChar();
        }

        /// <summary>
        /// Read a character on the input port.
        /// </summary>
        /// <returns>The character read.</returns>
        private ISchemeObject ReadChar()
        {
            var sb = new StringBuilder();
            ISchemeObject expr = this.parser.ReadChar(sb);
            this.transcript.LogInputLine(sb.ToString().Trim(), this);
            return expr;
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extensions for InputPort
    /// </summary>
    public static class InputPortExtension
    {
        /// <summary>
        /// Convert to input port.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>An input port.</returns>
        public static InputPort AsInputPort(this ISchemeObject obj)
        {
            if (obj is InputPort)
            {
                return (InputPort)obj;
            }

            ErrorHandlers.TypeError(typeof(InputPort), obj);
            return null;
        }

        /// <summary>
        /// Convert to text writer.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The text reader.</returns>
        public static TextReader AsTextReader(this ISchemeObject obj)
        {
            if (obj is InputPort)
            {
                return ((InputPort)obj).Parser.Reader;
            }

            ErrorHandlers.TypeError(typeof(InputPort), obj);
            return null;
        }
    }
    #endregion
}