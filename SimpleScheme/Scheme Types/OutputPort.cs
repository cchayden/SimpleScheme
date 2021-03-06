﻿// <copyright file="OutputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.IO;
    using System.Text;

    /// <summary>
    /// Writes to the output port.
    /// </summary>
    public class OutputPort : SchemeObject
    {
        #region Fields
        /// <summary>
        /// All output goes to this TextWriter.
        /// </summary>
        private readonly TextWriter outp;

        /// <summary>
        /// The logger
        /// </summary>
        private readonly TranscriptLogger transcript;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the OutputPort class.
        /// </summary>
        /// <param name="outp">The TextWriter to write output to.</param>
        /// <param name="interp">The interpreter.</param>
        private OutputPort(TextWriter outp, Interpreter interp)
        {
            this.outp = outp;
            this.transcript = interp.Transcript;
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the internal TextWriter object.
        /// </summary>
        internal TextWriter Writer
        { 
            get { return this.outp; }
        }

        /// <summary>
        /// True if writing to Console
        /// </summary>
        internal bool IsConsole
        {
            get { return this.outp == Console.Out; }
        }
        #endregion

        #region New
        /// <summary>
        /// Initializes a new instance of the OutputPort class.
        /// </summary>
        /// <param name="outp">The TextWriter to write output to.</param>
        /// <param name="interp">The interpreter.</param>
        /// <returns>A new output port</returns>
        public static OutputPort New(TextWriter outp, Interpreter interp)
        {
            return new OutputPort(outp, interp);
        }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the output primitives.
        /// </summary>
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-output-to-file <string> <thunk>)</r4rs>

            primEnv
                .DefinePrimitive(
                    "call-with-output-file",
                    new[] {"6.10.1", "(call-with-output-file <string> <proc>)"}, 
                    (args, env, caller) => EvaluateCallWithOutputFile.Call(args, caller), 
                    new ArgsInfo(2, ArgType.String, ArgType.Proc))
                .DefinePrimitive(
                    "close-output-port", new[] { "6.10.1", "(close-output-port <port>)" },
                    (args, env, caller) => Port(First(args), caller.Interp.CurrentOutputPort).CloseOutputPort(), 
                    new ArgsInfo(1, ArgType.OutputPort))
                .DefinePrimitive(
                    "current-output-port", new[] { "6.10.1", "(current-output-port)" },
                    (args, env, caller) => caller.Interp.CurrentOutputPort, 
                    new ArgsInfo(0))
                .DefinePrimitive(
                    "display", new[] { "6.10.3", "(display <obj>)", "(display <obj> <port>)" },
                    (args, env, caller) => Port(Second(args), caller.Interp.CurrentOutputPort).Display(First(args)), 
                    new ArgsInfo(1, 2, ArgType.Obj, ArgType.OutputPort))
                .DefinePrimitive(
                    "newline", new[] { "6.10.3", "(newline)", "(newline <port>)" },
                    (args, env, caller) => Port(First(args), caller.Interp.CurrentOutputPort).Newline(), 
                    new ArgsInfo(0, 1, ArgType.OutputPort))
                .DefinePrimitive(
                    "open-output-file", new[] { "6.10.1", "(open-output-file <filename>)" },
                    (args, env, caller) => EvaluateCallWithOutputFile.OpenOutputFile(First(args), caller.Interp), 
                    new ArgsInfo(1, ArgType.String))
                .DefinePrimitive(
                    "output-port?", new[] { "6.10.1", "(output-port? <obj>)" },
                    (args, env, caller) => SchemeBoolean.Truth(First(args) is OutputPort), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "write", new[] { "6.10.3", "(write <obj>)", "(write <obj> <port>)" },
                    (args, env, caller) => Port(Second(args), caller.Interp.CurrentOutputPort).Write(First(args)), 
                    new ArgsInfo(1, 2, ArgType.Obj, ArgType.OutputPort))
                .DefinePrimitive(
                    "p", new[] { "(p <expr>)", "(p <expr> <port>)" },
                    (args, env, caller) => Port(Second(args), caller.Interp.CurrentOutputPort).P(First(args)), 
                    new ArgsInfo(1, ArgType.Obj, ArgType.OutputPort))
                .DefinePrimitive(
                    "write-char", new[] { "6.10.3", "(write-char <char>)", "(write-char> <char> <port>)" },
                    (args, env, caller) => Port(Second(args), caller.Interp.CurrentOutputPort).WriteChar(First(args)), 
                    new ArgsInfo(1, 2, ArgType.Char, ArgType.OutputPort))
                .DefinePrimitive(
                    "dump-env", new[] { "(dump-env)" },
                    (args, env, caller) => DumpEnv(caller.Env), 
                    new ArgsInfo(0));
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write a string to the output port, followed by a newline.
        /// </summary>
        /// <param name="str">The string to write.</param>
        internal void WriteLine(string str)
        {
            this.outp.Write(str + this.outp.NewLine);
            this.transcript.LogOutputLine("=> " + str, this);
        }

        /// <summary>
        /// Write a string to the output port, NOT followed by a newline.
        /// </summary>
        /// <param name="str">The string to write.</param>
        internal void Write(string str)
        {
            this.outp.Write(str);
            this.transcript.LogOutput(str, this);
        }

        /// <summary>
        /// Close the output port.
        /// </summary>
        internal void Close()
        {
            this.outp.Close();
        }

        /// <summary>
        /// Flush the output waiting on the output port.
        /// </summary>
        internal void Flush()
        {
            this.outp.Flush();
        }

        /// <summary>
        /// Display the output port as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The output port type name.</returns>
        public override string ToString()
        {
            return "<output-port>";
        }
        #endregion

        #region CLR Type Converters
        /// <summary>
        /// Convert to text writer
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The text writer.</returns>
        internal static TextWriter AsTextWriter(SchemeObject obj)
        {
            if (obj is OutputPort)
            {
                return ((OutputPort)obj).Writer;
            }

            ErrorHandlers.TypeError(typeof(OutputPort), obj);
            return null;
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Determine the port object to use with the OutputPort primitives.
        /// The port is optional: if supplied, it is the port to use.
        /// Otherwise, the current output port is used instead.
        /// </summary>
        /// <param name="port">The port to use, if supplied.  If is is not empty list, then it is an output port.</param>
        /// <param name="curr">The current output port.</param>
        /// <returns>The port to use.</returns>
        private static OutputPort Port(SchemeObject port, OutputPort curr)
        {
            return port is EmptyList ? curr : (OutputPort)port;
        }

        /// <summary>
        /// Dump the environment on standard output.
        /// </summary>
        /// <param name="env">The environment to dump.</param>
        /// <returns>Undefined instance.</returns>
        private static SchemeObject DumpEnv(Environment env)
        {
            env.DumpEnv();
            return Undefined.Instance;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Print the obj on the console.
        /// </summary>
        /// <param name="x">The obj to print.</param>
        /// <returns>Undefined value.</returns>
        private SchemeObject P(SchemeObject x)
        {
            this.WriteLine(x.ToString(false));
            return Undefined.Instance;
        }

        /// <summary>
        /// Write an expression on a given port.
        /// The output is not quoted.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <returns>The undefined object.</returns>
        private SchemeObject Display(SchemeObject expr)
        {
            this.Write(expr.ToString(false));
            return Undefined.Instance;
        }

        /// <summary>
        /// Write an expression on a given port.
        /// The output is quoted.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <returns>The undefined object.</returns>
        private SchemeObject Write(SchemeObject expr)
        {
            this.Write(expr.ToString(true));
            return Undefined.Instance;
        }

        /// <summary>
        /// Write a character on a given port.
        /// The output is not quoted.
        /// If the expr is not actually a character, it is written nevertheless.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <returns>The undefined object.</returns>
        private SchemeObject WriteChar(SchemeObject expr)
        {
            this.Write(expr.ToString(false));
            return Undefined.Instance;
        }

        /// <summary>
        /// Close the output port.
        /// </summary>
        /// <returns>Undefined instance.</returns>
        private SchemeObject CloseOutputPort()
        {
            this.Close();
            return Undefined.Instance;
        }

        /// <summary>
        /// Display a newline on the output port.
        /// </summary>
        /// <returns>Undefined instance.</returns>
        private SchemeObject Newline()
        {
            this.Write(this.outp.NewLine);
            return Undefined.Instance;
        }
        #endregion
    }
}