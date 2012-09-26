﻿// <copyright file="OutputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;

    /// <summary>
    /// Writes to the output port.
    /// </summary>
    public class OutputPort : IPrintable, ISchemeObject
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

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Port); }
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the internal TextWriter object.
        /// </summary>
        public TextWriter Writer
        { 
            get { return this.outp; }
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
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-output-to-file <string> <thunk>)</r4rs>

            env
                //// <r4rs section="6.10.1">(call-with-output-file <string> <proc>)</r4rs>
                .DefinePrimitive(
                    "call-with-output-file",
                    EvaluateCallWithOutputFile.Call, 
                    2, 
                    TypePrimitives.ValueType.String, 
                    TypePrimitives.ValueType.Proc)
                //// <r4rs section="6.10.1">(close-output-port <port>)</r4rs>
                .DefinePrimitive(
                    "close-output-port",
                    (args, caller) => Port(List.First(args), caller.Interp.CurrentOutputPort).CloseOutputPort(), 
                    1, 
                    TypePrimitives.ValueType.Port)
                //// <r4rs section="6.10.1">(current-output-port)</r4rs>
                .DefinePrimitive(
                    "current-output-port",
                    (args, caller) => caller.Interp.CurrentOutputPort, 
                    0)
                //// <r4rs section="6.10.3">(display <obj>)</r4rs>
                //// <r4rs section="6.10.3">(display <obj> <port>)</r4rs>
                .DefinePrimitive(
                    "display",
                    (args, caller) => Port(List.Second(args), caller.Interp.CurrentOutputPort).Display(List.First(args)), 
                    1, 
                    2,
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Port)
                //// <r4rs section="6.10.3">(newline)</r4rs>
                //// <r4rs section="6.10.3">(newline <port>)</r4rs>
                .DefinePrimitive(
                    "newline",
                    (args, caller) => Port(List.First(args), caller.Interp.CurrentOutputPort).Newline(), 
                    0, 
                    1, 
                    TypePrimitives.ValueType.Port)
                //// <r4rs section="6.10.1">(open-output-file <filename>)</r4rs>
                .DefinePrimitive(
                    "open-output-file",
                    (args, caller) => EvaluateCallWithOutputFile.OpenOutputFile(List.First(args), caller.Interp), 
                    1, 
                    TypePrimitives.ValueType.String)
                //// <r4rs section="6.10.1">(output-port? <obj>)</r4rs>
                .DefinePrimitive(
                    "output-port?",
                    (args, caller) => SchemeBoolean.Truth(List.First(args) is OutputPort), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.10.3">(write <obj>)</r4rs>
                //// <r4rs section="6.10.3">(write <obj> <port>)</r4rs>
                .DefinePrimitive(
                    "write",
                    (args, caller) => Port(List.Second(args), caller.Interp.CurrentOutputPort).Write(List.First(args)), 
                    1, 
                    2, 
                    TypePrimitives.ValueType.Obj, 
                    TypePrimitives.ValueType.Port)
                //// (p <expr>)
                .DefinePrimitive(
                    "p",
                    (args, caller) => Port(List.Second(args), caller.Interp.CurrentOutputPort).P(List.First(args)), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.10.3">(write-char <char>)</r4rs>
                //// <r4rs section="6.10.3">(write-char> <char> <port>)</r4rs>
                .DefinePrimitive(
                    "write-char",
                    (args, caller) => Port(List.Second(args), caller.Interp.CurrentOutputPort).WriteChar(List.First(args)), 
                    1, 
                    2,
                    TypePrimitives.ValueType.Char, 
                    TypePrimitives.ValueType.Port)
                //// (dump-env)
                .DefinePrimitive(
                    "dump-env",
                    (args, caller) => DumpEnv(caller.Env), 
                    0);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the output port to the string builder.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            buf.Append(this.ToString());
        }

        /// <summary>
        /// Write a string to the output port, followed by a newline.
        /// </summary>
        /// <param name="str">The string to write.</param>
        public void WriteLine(string str)
        {
            this.outp.Write(str + this.outp.NewLine);
            this.transcript.LogOutputLine("=> " + str, this);
        }

        /// <summary>
        /// Write a string to the output port, NOT followed by a newline.
        /// </summary>
        /// <param name="str">The string to write.</param>
        public void Write(string str)
        {
            this.outp.Write(str);
            this.transcript.LogOutput(str, this);
        }

        /// <summary>
        /// Close the output port.
        /// </summary>
        public void Close()
        {
            this.outp.Close();
        }

        /// <summary>
        /// Flush the output waiting on the output port.
        /// </summary>
        public void Flush()
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

        #region Private Static Methods
        /// <summary>
        /// Determine the port object to use with the OutputPort primitives.
        /// The port is optional: if supplied, it is the port to use.
        /// Otherwise, the current output port is used instead.
        /// </summary>
        /// <param name="port">The port to use, if supplied.</param>
        /// <param name="curr">The current output port.</param>
        /// <returns>The port to use.</returns>
        private static OutputPort Port(ISchemeObject port, OutputPort curr)
        {
            return port is EmptyList ? curr : port.AsOutputPort();
        }

        /// <summary>
        /// Dump the environment on standard output.
        /// </summary>
        /// <param name="env">The environment to dump.</param>
        /// <returns>Undefined instance.</returns>
        private static ISchemeObject DumpEnv(Environment env)
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
        private ISchemeObject P(ISchemeObject x)
        {
            this.WriteLine(Printer.AsString(x, false));
            return Undefined.Instance;
        }

        /// <summary>
        /// Write an expression on a given port.
        /// The output is not quoted.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <returns>The undefined object.</returns>
        private ISchemeObject Display(ISchemeObject expr)
        {
            this.Write(Printer.AsString(expr, false));
            return Undefined.Instance;
        }

        /// <summary>
        /// Write an expression on a given port.
        /// The output is quoted.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <returns>The undefined object.</returns>
        private ISchemeObject Write(ISchemeObject expr)
        {
            this.Write(Printer.AsString(expr, true));
            return Undefined.Instance;
        }

        /// <summary>
        /// Write a character on a given port.
        /// The output is not quoted.
        /// If the expr is not actually a character, it is written nevertheless.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <returns>The undefined object.</returns>
        private ISchemeObject WriteChar(ISchemeObject expr)
        {
            this.Write(Printer.AsString(expr, false));
            return Undefined.Instance;
        }

        /// <summary>
        /// Close the output port.
        /// </summary>
        /// <returns>Undefined instance.</returns>
        private ISchemeObject CloseOutputPort()
        {
            this.Close();
            return Undefined.Instance;
        }

        /// <summary>
        /// Display a newline on the output port.
        /// </summary>
        /// <returns>Undefined instance.</returns>
        private ISchemeObject Newline()
        {
            this.Write(this.outp.NewLine);
            return Undefined.Instance;
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extension class for OutputPort.
    /// </summary>
    public static class OutputPortExtension
    {
        /// <summary>
        /// Convert to output port
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The output port.</returns>
        public static OutputPort AsOutputPort(this ISchemeObject obj)
        {
            if (obj is OutputPort)
            {
                return (OutputPort)obj;
            }

            ErrorHandlers.TypeError(typeof(OutputPort), obj);
            return null;
        }

        /// <summary>
        /// Convert to text writer
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The text writer.</returns>
        public static TextWriter AsTextWriter(this ISchemeObject obj)
        {
            if (obj is OutputPort)
            {
                return ((OutputPort)obj).Writer;
            }

            ErrorHandlers.TypeError(typeof(OutputPort), obj);
            return null;
        }
    }
    #endregion
}