﻿// <copyright file="InputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.IO;
    using System.Text;

    /// <summary>
    /// Represents an input port, a mechanism for reading input.
    /// It is immutable.
    /// </summary>
    public sealed class InputPort : SchemeObject
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
        /// All input fomes from this TextReader.
        /// </summary>
        private readonly TextReader inp;

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
            this.inp = inp;
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

        /// <summary>
        /// Gets the line that the parser is on now.
        /// </summary>
        internal int ParserLineNumber
        {
            get { return this.parser.LineNumber; }
        }

        /// <summary>
        /// True if reading from Console
        /// </summary>
        internal bool IsConsole
        {
            get { return this.inp == Console.In;}
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
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-input-from-file <string> <thunk>)</r4rs>
            // TODO implement char-ready?
            //// <r4rs section="6.10.2">(char-ready?)</r4rs>
            //// <r4rs section="6.10.2">(char-ready? <port>)</r4rs>

            primEnv
                .DefinePrimitive(
                        "eof-object?", 
                        new[] { "6.10.2", "(eof-object? <obj>)" },
                        (args, env, caller) => SchemeBoolean.Truth(First(args) is Eof), 
                        new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                       "call-with-input-file", 
                       new[] { "6.10.1", "(call-with-input-file <string> <proc>)" },
                       (args, env, caller) => EvaluateCallWithInputFile.Call(args, caller), 
                       new ArgsInfo(2, ArgType.String, ArgType.Proc))
                .DefinePrimitive(
                        "close-input-port", 
                        new[] { "6.10.1", "(close-input-port <port>)" },
                        (args, env, caller) => Port(First(args), caller.Interp.CurrentInputPort).CloseInputPort(), 
                        new ArgsInfo(1, ArgType.InputPort))
                .DefinePrimitive(
                        "current-input-port", 
                        new[] { "6.10.1", "(current-input-port)" },
                        (args, env, caller) => caller.Interp.CurrentInputPort, 
                        new ArgsInfo(0))
                .DefinePrimitive(
                        "input-port?", 
                        new[] { "6.10.1", "(input-port? <obj>)" },
                        (args, env, caller) => SchemeBoolean.Truth(First(args) is InputPort), 
                        new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                        "load", 
                        new[] { "6.10.4", "(load <filename>)" },
                        (args, env, caller) => LoadFile(First(args), caller.Interp), 
                        new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                        "open-input-file", 
                        new[] { "6.10.1", "(open-input-file <filename>)" },
                        (args, env, caller) => EvaluateCallWithInputFile.OpenInputFile(First(args), caller.Interp), 
                        new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                        "peek-char", 
                        new[] { "6.10.2", "(peek-char)", "(peek-char <port>)" },
                        (args, env, caller) => Port(First(args), caller.Interp.CurrentInputPort).PeekChar(), 
                        new ArgsInfo(0, 1, ArgType.InputPort))
                .DefinePrimitive(
                        "read", 
                        new[] { "6.10.2", "(read)", "(read <port>)" },
                        (args, env, caller) => Port(First(args), caller.Interp.CurrentInputPort).Read(), 
                        new ArgsInfo(0, 1, ArgType.InputPort))
                .DefinePrimitive(
                        "read-char", 
                        new[] { "6.10.2", "(read-char)", "(read-char <port>)" },
                        (args, env, caller) => Port(First(args), caller.Interp.CurrentInputPort).ReadChar(), 
                        new ArgsInfo(0, 1, ArgType.InputPort))
                .DefinePrimitive(
                        "transcript-on", 
                        new[] { "6.10.4", "(transcript-on <filename>)" },
                        (args, env, caller) => TranscriptOn(First(args), caller.Interp.Transcript), 
                        new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                        "transcript-off", 
                        new[] { "6.10.4", "(transcript-off)" },
                        (args, env, caller) => TranscriptOff(caller.Interp.Transcript), 
                        new ArgsInfo(0));
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the input port as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The input port type name.</returns>
        public override string ToString()
        {
            return "<input-port>";
        }

        /// <summary>
        /// Read an expression from the given input port.
        /// If none given, uses the default input port.
        /// </summary>
        /// <returns>The expression read.</returns>
        internal SchemeObject Read()
        {
            var sb = new StringBuilder();
            SchemeObject expr = this.parser.ReadExpr(sb);
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

        #region CLR Type Converters
        /// <summary>
        /// Convert to text writer.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The text reader.</returns>
        internal static TextReader AsTextReader(SchemeObject obj)
        {
            if (obj is InputPort)
            {
                return ((InputPort)obj).Parser.Reader;
            }

            ErrorHandlers.TypeError(typeof(InputPort), obj);
            return null;
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
        private static InputPort Port(SchemeObject port, InputPort curr)
        {
            return port is EmptyList ? curr : (InputPort)port;
        }

        /// <summary>
        /// Load a file given the filename.
        /// </summary>
        /// <param name="filename">The file to load from.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>Undefined object.</returns>
        private static SchemeObject LoadFile(SchemeObject filename, Interpreter interp)
        {
            interp.LoadFile(filename, null);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript on.
        /// </summary>
        /// <param name="filename">The file to write to.</param>
        /// <param name="transcript">The transcript logger.</param>
        /// <returns>Undefined object.</returns>
        private static SchemeObject TranscriptOn(SchemeObject filename, TranscriptLogger transcript)
        {
            transcript.TranscriptOn(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript off.
        /// </summary>
        /// <param name="transcript">The transcript logger.</param>
        /// <returns>Undefined object.</returns>
        private static SchemeObject TranscriptOff(TranscriptLogger transcript)
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
        private SchemeObject CloseInputPort()
        {
            this.Close();
            return Undefined.Instance;
        }

        /// <summary>
        /// Peek for a character on the input port.
        /// </summary>
        /// <returns>The character in the input.</returns>
        private SchemeObject PeekChar()
        {
            return this.parser.PeekChar();
        }

        /// <summary>
        /// Read a character on the input port.
        /// </summary>
        /// <returns>The character read.</returns>
        private SchemeObject ReadChar()
        {
            SchemeObject expr = this.parser.ReadChar();
            if (expr is Character)
            {
                this.transcript.LogInputLine(expr.ToString(), this);
            }
            return expr;
        }
        #endregion
    }
}