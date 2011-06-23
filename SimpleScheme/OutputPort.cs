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
    public sealed class OutputPort : ListPrimitives
    {
        #region Fields
        /// <summary>
        /// The output port to write to.
        /// </summary>
        private readonly TextWriter outp;
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the OutputPort class.
        /// </summary>
        /// <param name="outp">The output port to use for writing.</param>
        private OutputPort(TextWriter outp)
        {
            this.outp = outp;
        }
        #endregion

        #region Oublic Static Methods.
        /// <summary>
        /// Creates a new OutputPort.
        /// </summary>
        /// <param name="outp">The output port to use for writing.</param>
        /// <returns>The new OutputPort.</returns>
        public static OutputPort New(TextWriter outp)
        {
            return new OutputPort(outp);
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the output primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-output-to-file <string> <thunk>)</r4rs>

            env
                //// <r4rs section="6.10.1">(call-with-output-file <string> <proc>)</r4rs>
                .DefinePrimitive("call-with-output-file", (args, caller) => EvaluateCallWithOutputFile.Call(args, caller), 2)
                //// <r4rs section="6.10.1">(close-output-port <port>)</r4rs>
                .DefinePrimitive("close-output-port", (args, caller) => CloseOutputPort(First(args), caller), 1)
                //// <r4rs section="6.10.1">(current-output-port)</r4rs>
                .DefinePrimitive("current-output-port", (args, caller) => caller.Env.Interp.Output, 0)
                //// <r4rs section="6.10.3">(display <obj>)</r4rs>
                //// <r4rs section="6.10.3">(display <obj> <port>)</r4rs>
                .DefinePrimitive(
                    "display",
                    (args, caller) => Write(First(args), OutPort(Second(args), caller.Env.Interp.Output), false),
                    1, 
                    2)
                //// <r4rs section="6.10.3">(newline)</r4rs>
                //// <r4rs section="6.10.3">(newline <port>)</r4rs>
                .DefinePrimitive("newline", (args, caller) => Newline(First(args), caller), 0, 1)
                //// <r4rs section="6.10.1">(open-output-file <filename>)</r4rs>
                .DefinePrimitive("open-output-file", (args, caller) => EvaluateCallWithOutputFile.OpenOutputFile(First(args)), 1)
                //// <r4rs section="6.10.1">(output-port? <obj>)</r4rs>
                .DefinePrimitive("output-port?", (args, caller) => SchemeBoolean.Truth(First(args) is OutputPort), 1)
                //// <r4rs section="6.10.3">(write <obj>)</r4rs>
                //// <r4rs section="6.10.3">(write <obj> <port>)</r4rs>
                .DefinePrimitive(
                   "write",
                   (args, caller) => Write(First(args), OutPort(Second(args), caller.Env.Interp.Output), true), 
                   1,
                   2)
                //// (p <expr>)
                .DefinePrimitive("p", (args, caller) => P(First(args)), 1)
                //// <r4rs section="6.10.3">(write-char <char>)</r4rs>
                //// <r4rs section="6.10.3">(write-char> <char> <port>)</r4rs>
                .DefinePrimitive(
                    "write-char",
                    (args, caller) => Write(First(args), OutPort(Second(args), caller.Env.Interp.Output), false),
                    1,
                    2)
                //// (dump-env)
                .DefinePrimitive("dump-env", (args, caller) => DumpEnv(caller), 0);
        }
        #endregion

        #region Internal Methods
        /// <summary>
        /// Write the given expression on the curent port, with quoting.
        /// </summary>
        /// <param name="expr">The expression to write.</param>
        internal void Write(Obj expr)
        {
            Write(expr, this, true);
        }

        /// <summary>
        /// Close the output port.
        /// </summary>
        internal void Close()
        {
            this.outp.Close();
        }

        /// <summary>
        /// Flush the output port.
        /// </summary>
        internal void Flush()
        {
            this.outp.Flush();
        }

        /// <summary>
        /// Print the obj on the output port.
        /// </summary>
        /// <param name="expr">The obj to print.</param>
        internal void Print(Obj expr)
        {
            this.outp.Write(expr);
        }

        /// <summary>
        /// Print a newline to the output port.
        /// </summary>
        internal void Println()
        {
            this.outp.WriteLine();
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Convert an obj into an output port.
        /// If the obj is the empty list, then return the interpreter's outpot port.
        /// </summary>
        /// <param name="expr">The obj (should be an output port).</param>
        /// <param name="outPort">The output port to use if the port is the empty list.</param>
        /// <returns>An output port.</returns>
        private static OutputPort OutPort(Obj expr, OutputPort outPort)
        {
            if (expr == List.Empty)
            {
                return outPort;
            }

            if (expr is OutputPort)
            {
                return (OutputPort)expr;
            }

            return OutPort(ErrorHandlers.Error("Expected an output port, got: " + expr), null);
        }

        /// <summary>
        /// Write an obj into an output port.
        /// </summary>
        /// <param name="expr">The obj to write.</param>
        /// <param name="port">The output port.</param>
        /// <param name="quoted">Whether to quote strings and chars.</param>
        /// <returns>Undefined value.</returns>
        private static Obj Write(Obj expr, OutputPort port, bool quoted)
        {
            port.Print(SchemeString.AsString(expr, quoted));
            port.Flush();
            return Undefined.Instance;
        }

        /// <summary>
        /// Print the obj on the console.
        /// </summary>
        /// <param name="x">The obj to print.</param>
        /// <returns>Undefined value.</returns>
        private static Obj P(Obj x)
        {
            Console.Out.WriteLine(SchemeString.AsString(x, false));
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
            OutPort(port, caller.Env.Interp.Output).Close();
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
            OutPort(port, caller.Env.Interp.Output).Println();
            OutPort(port, caller.Env.Interp.Output).Flush();
            return Undefined.Instance;
        }

        /// <summary>
        /// Dump the environment on standard output.
        /// </summary>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined instance.</returns>
        private static Obj DumpEnv(Stepper caller)
        {
            caller.Env.DumpEnv();
            return Undefined.Instance;
        }

        #endregion
    }
}