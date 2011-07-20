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
    public static class OutputPort
    {
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
        private static TextWriter OutPort(Obj obj)
        {
            if (TypePrimitives.IsOutputPort(obj))
            {
                return (TextWriter)obj;
            }

            ErrorHandlers.TypeError(TypePrimitives.OutputPortName, obj);
            return null;
        }

        /// <summary>
        /// Write an obj into an output port.
        /// </summary>
        /// <param name="expr">The obj to write.</param>
        /// <param name="port">The output port.</param>
        /// <param name="quoted">Whether to quote strings and chars.</param>
        /// <returns>Undefined value.</returns>
        private static Obj WriteObj(Obj expr, TextWriter port, bool quoted)
        {
            port.Write(Printer.AsString(expr, quoted));
            port.Flush();
            return Undefined.Instance;
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
            TextWriter p = TypePrimitives.IsEmptyList(port) ? caller.CurrentOutputPort : OutPort(port);
            return WriteObj(expr, p, false);
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
            TextWriter p = TypePrimitives.IsEmptyList(port) ? caller.CurrentOutputPort : OutPort(port);
            return WriteObj(expr, p, true);
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
            TextWriter p = TypePrimitives.IsEmptyList(port) ? caller.CurrentOutputPort : OutPort(port);
            return WriteObj(expr, p, false);
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
            TextWriter p = TypePrimitives.IsEmptyList(port) ? caller.CurrentOutputPort : OutPort(port);
            p.Close();
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
            TextWriter p = TypePrimitives.IsEmptyList(port) ? caller.CurrentOutputPort : OutPort(port);
            p.WriteLine();
            p.Flush();
            return Undefined.Instance;
        }

        /// <summary>
        /// Dump the environment on standard output.
        /// </summary>
        /// <param name="caller">The caller.</param>
        /// <returns>Undefined instance.</returns>
        private static Obj DumpEnv(Stepper caller)
        {
            caller.Env.DumpEnv(caller.CurrentOutputPort);
            return Undefined.Instance;
        }

        #endregion
    }
}