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
    public sealed class InputPort
    {
        #region Fields
        /// <summary>
        /// Marks the end of the input file.
        /// </summary>
        internal const string Eof = "#!EOF";

        /// <summary>
        /// The input comes from this TextReader.
        /// </summary>
        private readonly TextReader inp;

        /// <summary>
        /// This is used to parse the input.
        /// It holds a little state: a single character or a single token read-ahead.
        /// </summary>
        private readonly Parser parser = new Parser();
        #endregion

        #region Construcor
        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// Store the TextReader and initialize the parser.
        /// </summary>
        /// <param name="inp">A text reader.</param>
        internal InputPort(TextReader inp)
        {
            this.inp = inp;
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
                .DefinePrimitive("close-input-port", (args, caller) => CloseInputPort(List.First(args), caller), 1)
                //// <r4rs section="6.10.1">(current-input-port)</r4rs>
                .DefinePrimitive("current-input-port", (args, caller) => caller.CurrentInputPort, 0)
                //// <r4rs section="6.10.1">(input-port? <obj>)</r4rs>
                .DefinePrimitive("input-port?", (args, caller) => SchemeBoolean.Truth(TypePrimitives.IsInputPort(List.First(args))), 1)
                //// <r4rs section="6.10.4">(load <filename>)</r4rs>
                .DefinePrimitive("load", (args, caller) => LoadFile(List.First(args), caller), 1)
                //// <r4rs section="6.10.1">(open-input-file <filename>)</r4rs>
                .DefinePrimitive("open-input-file", (args, caller) => EvaluateCallWithInputFile.OpenInputFile(List.First(args)), 1)
                //// <r4rs section="6.10.2">(peek-char)</r4rs>
                //// <r4rs section="6.10.2">(peek-char <port>)</r4rs>
                .DefinePrimitive("peek-char", (args, caller) => PeekChar(List.First(args), caller), 0, 1)
                //// <r4rs section="6.10.2">(read)</r4rs>
                //// <r4rs section="6.10.2">(read <port>)</r4rs>
                .DefinePrimitive("read", (args, caller) => Read(List.First(args), caller), 0, 1)
                //// <r4rs section="6.10.2">(read-char)</r4rs>
                //// <r4rs section="6.10.2">(read-char <port>)</r4rs>
                .DefinePrimitive("read-char", (args, caller) => ReadChar(List.First(args), caller), 0, 1)
                //// <r4rs section="6.10.4">(transcript-on <filename>)</r4rs>
                .DefinePrimitive("transcript-on", (args, caller) => TranscriptOn(List.First(args), caller), 1)
                //// <r4rs section="6.10.4">(transcript-off)</r4rs>
                .DefinePrimitive("transcript-off", (args, caller) => TranscriptOff(caller), 0);
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
            return this.parser.Read(this.inp);
        }

        /// <summary>
        /// Close the input port.
        /// Closes the stream used by the parser.
        /// </summary>
        internal void Close()
        {
            try
            {
                this.inp.Close();
            }
            catch (IOException ex)
            {
                ErrorHandlers.IoError("IOException on close: " + ex);
            }
        }
        #endregion

        #region Private Static Methods
        private static InputPort Port(Obj port, Stepper caller)
        {
            return TypePrimitives.IsEmptyList(port) ? caller.CurrentInputPort : InPort(port);
        }

        /// <summary>
        /// Load a file given the filename.
        /// </summary>
        /// <param name="filename">The file to load from.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>Undefined object.</returns>
        private static object LoadFile(Obj filename, Stepper caller)
        {
            caller.LoadFile(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript on.
        /// </summary>
        /// <param name="filename">The file to write to.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>Undefined object.</returns>
        private static object TranscriptOn(Obj filename, Stepper caller)
        {
            caller.TranscriptOn(filename);
            return Undefined.Instance;
        }

        /// <summary>
        /// Turn the transcript off.
        /// </summary>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>Undefined object.</returns>
        private static object TranscriptOff(Stepper caller)
        {
            caller.TranscriptOff();
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
            InputPort p = Port(port, caller);
            p.Close();
            return Undefined.Instance;
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
            InputPort p = Port(port, caller);
            return p.parser.PeekChar(p.inp);
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
            InputPort p = Port(port, caller);
            return p.parser.Read(p.inp);
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
            InputPort p = Port(port, caller);
            return p.parser.ReadChar(p.inp);
        }
        #endregion
    }
}