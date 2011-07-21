// <copyright file="OutputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.IO;
    using Obj = System.Object;

    /// <summary>
    /// Writes to the output port.
    /// </summary>
    public class OutputPort
    {
        #region Fields
        /// <summary>
        /// All output goes to this TextWriter.
        /// </summary>
        private readonly TextWriter outp;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the OutputPort class.
        /// </summary>
        /// <param name="outp">The TextWriter to write output to.</param>
        public OutputPort(TextWriter outp)
        {
            this.outp = outp;
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Write a string to the output port, followed by a newline.
        /// </summary>
        /// <param name="str">The string to write.</param>
        internal void WriteLine(string str)
        {
            this.outp.Write(str + this.outp.NewLine);
        }

        /// <summary>
        /// Write a newline to the output port.
        /// </summary>
        internal void WriteLine()
        {
            this.outp.Write(this.outp.NewLine);
        }

        /// <summary>
        /// Write a string to the output port, NOT followed by a newline.
        /// </summary>
        /// <param name="str">The string to write.</param>
        internal void Write(string str)
        {
            this.outp.Write(str);
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
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the output primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(PrimitiveEnvironment env)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-output-to-file <string> <thunk>)</r4rs>

            env
                //// <r4rs section="6.10.1">(call-with-output-file <string> <proc>)</r4rs>
                .DefinePrimitive("call-with-output-file", (args, caller) => EvaluateCallWithOutputFile.Call(args, caller), 2)
                //// <r4rs section="6.10.1">(close-output-port <port>)</r4rs>
                .DefinePrimitive("close-output-port", (args, caller) => CloseOutputPort(List.First(args), caller), 1)
                //// <r4rs section="6.10.1">(current-output-port)</r4rs>
                .DefinePrimitive("current-output-port", (args, caller) => caller.CurrentOutputPort, 0)
                //// <r4rs section="6.10.3">(display <obj>)</r4rs>
                //// <r4rs section="6.10.3">(display <obj> <port>)</r4rs>
                .DefinePrimitive("display", (args, caller) => Display(List.First(args), List.Second(args), caller), 1, 2)
                //// <r4rs section="6.10.3">(newline)</r4rs>
                //// <r4rs section="6.10.3">(newline <port>)</r4rs>
                .DefinePrimitive("newline", (args, caller) => Newline(List.First(args), caller), 0, 1)
                //// <r4rs section="6.10.1">(open-output-file <filename>)</r4rs>
                .DefinePrimitive("open-output-file", (args, caller) => EvaluateCallWithOutputFile.OpenOutputFile(List.First(args)), 1)
                //// <r4rs section="6.10.1">(output-port? <obj>)</r4rs>
                .DefinePrimitive("output-port?", (args, caller) => SchemeBoolean.Truth(TypePrimitives.IsOutputPort(List.First(args))), 1)
                //// <r4rs section="6.10.3">(write <obj>)</r4rs>
                //// <r4rs section="6.10.3">(write <obj> <port>)</r4rs>
                .DefinePrimitive("write", (args, caller) => Write(List.First(args), List.Second(args), caller), 1, 2)
                //// (p <expr>)
                .DefinePrimitive("p", (args, caller) => P(List.First(args)), 1)
                //// <r4rs section="6.10.3">(write-char <char>)</r4rs>
                //// <r4rs section="6.10.3">(write-char> <char> <port>)</r4rs>
                .DefinePrimitive("write-char", (args, caller) => WriteChar(List.First(args), List.Second(args), caller), 1, 2)
                //// (dump-env)
                .DefinePrimitive("dump-env", (args, caller) => DumpEnv(caller), 0);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Check that the given object is an output port.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>The output port.</returns>
        private static OutputPort OutPort(Obj obj)
        {
            if (TypePrimitives.IsOutputPort(obj))
            {
                return (OutputPort)obj;
            }

            ErrorHandlers.TypeError(TypePrimitives.OutputPortName, obj);
            return null;
        }

        /// <summary>
        /// Determine the port object to use with the OutputPort primitives.
        /// The port is optional: if supplied, it is the port to use.
        /// Otherwise, the current output port is used instead.
        /// </summary>
        /// <param name="port">The port to use, if supplied.</param>
        /// <param name="caller">The caller, from which the current output port can be obtained.</param>
        /// <returns>The port to use.</returns>
        private static OutputPort Port(Obj port, Stepper caller)
        {
            return TypePrimitives.IsEmptyList(port) ? caller.CurrentOutputPort : OutPort(port);
        }

        /// <summary>
        /// Write an obj into an output port.
        /// </summary>
        /// <param name="str">The string to write.</param>
        /// <param name="port">The output port.</param>
        private static void WriteObj(string str, OutputPort port)
        {
            port.Write(str);
            port.Flush();
        }

        /// <summary>
        /// Write an expression on a given port, or onto the default port.
        /// The output is not quoted.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <param name="port">The port, or the empty list.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The undefined object.</returns>
        private static Obj Display(Obj expr, Obj port, Stepper caller)
        {
            string output = Printer.AsString(expr, false);
            OutputPort p = Port(port, caller);
            WriteObj(output, p);
            caller.LogOutput(output, p);
            return Undefined.Instance;
        }

        /// <summary>
        /// Write an expression on a given port, or onto the default port.
        /// The output is quoted.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <param name="port">The port, or the empty list.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The undefined object.</returns>
        private static Obj Write(Obj expr, Obj port, Stepper caller)
        {
            string output = Printer.AsString(expr, true);
            OutputPort p = Port(port, caller);
            WriteObj(output, p);
            caller.LogOutput(output, p);
            return Undefined.Instance;
        }

        /// <summary>
        /// Write a character on a given port, or onto the default port.
        /// The output is not quoted.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        /// <param name="port">The port, or the empty list.</param>
        /// <param name="caller">The calling stepper.</param>
        /// <returns>The undefined object.</returns>
        private static Obj WriteChar(Obj expr, Obj port, Stepper caller)
        {
            string output = Printer.AsString(expr, false);
            OutputPort p = Port(port, caller);
            WriteObj(output, p);
            caller.LogOutput(output, p);
            return Undefined.Instance;
        }

        /// <summary>
        /// Print the obj on the console.
        /// </summary>
        /// <param name="x">The obj to print.</param>
        /// <returns>Undefined value.</returns>
        private static Obj P(Obj x)
        {
            Console.Out.WriteLine(Printer.AsString(x, false));
            return Undefined.Instance;
        }

        /// <summary>
        /// Close the output port.
        /// </summary>
        /// <param name="port">The port to close.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined instance.</returns>
        private static Obj CloseOutputPort(Obj port, Stepper caller)
        {
            Port(port, caller).Close();
            return Undefined.Instance;
        }

        /// <summary>
        /// Display a newline on the output.
        /// </summary>
        /// <param name="port">The port to write to.</param>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined instance.</returns>
        private static Obj Newline(Obj port, Stepper caller)
        {
            OutputPort p = Port(port, caller);
            WriteObj(p.outp.NewLine, p);
            caller.LogOutput(p.outp.NewLine, p);
            return Undefined.Instance;
        }

        /// <summary>
        /// Dump the environment on standard output.
        /// </summary>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined instance.</returns>
        private static Obj DumpEnv(Stepper caller)
        {
            caller.Env.DumpEnv(caller);
            return Undefined.Instance;
        }

        #endregion
    }
}