// <copyright file="OutputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.IO;

    /// <summary>
    /// Writes to the output port.
    /// </summary>
    public sealed class OutputPort
    {
        /// <summary>
        /// The output port to write to.
        /// </summary>
        private readonly TextWriter outp;

        /// <summary>
        /// Initializes a new instance of the OutputPort class.
        /// </summary>
        /// <param name="outp">The output port to use for writing.</param>
        public OutputPort(TextWriter outp)
        {
            this.outp = outp;
        }

        /// <summary>
        /// Convert an object into an output port.
        /// If the object is null, then return the interpreter's outpot port.
        /// </summary>
        /// <param name="x">The object (should be an output port).</param>
        /// <param name="interp">The interpreter to use if the port is null.</param>
        /// <returns>An output port.</returns>
        public static OutputPort OutPort(object x, Interpreter interp)
        {
            if (x == null)
            {
                return interp.Output;
            }

            if (x is OutputPort)
            {
                return (OutputPort)x;
            }

            return OutPort(ErrorHandlers.Error("Expected an output port, got: " + x), interp);
        }

        /// <summary>
        /// Define the output primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-input-from-file <string> <thunk>)</r4rs>

            env
                //// <r4rs section="6.10.1">(call-with-output-file <string> <proc>)</r4rs>
                .DefinePrimitive("call-with-output-file", (parent, args) => EvaluateCallWithOutputFile.Call(parent, args), 2)
                //// <r4rs section="6.10.1">(close-output-port <port>)</r4rs>
                .DefinePrimitive(
                   "close-output-port",
                   (parent, args) =>
                   {
                       OutPort(List.First(args), parent.Env.Interp).Close();
                       return SchemeBoolean.True;
                   },
                   1)
                //// <r4rs section="6.10.1">(current-output-port)</r4rs>
                .DefinePrimitive("current-output-port", (parent, args) => parent.Env.Interp.Output, 0)
                //// <r4rs section="6.10.3">(display <obj>)</r4rs>
                //// <r4rs section="6.10.3">(display <obj> <port>)</r4rs>
                .DefinePrimitive("display", (parent, args) => Write(List.First(args), OutPort(List.Second(args), parent.Env.Interp), false), 1, 2)
                //// <r4rs section="6.10.3">(newline)</r4rs>
                //// <r4rs section="6.10.3">(newline <port>)</r4rs>
                .DefinePrimitive(
                   "newline",
                   (parent, args) =>
                   {
                       object first = List.First(args);
                       OutPort(first, parent.Env.Interp).Println();
                       OutPort(first, parent.Env.Interp).Flush();
                       return SchemeBoolean.True;
                   },
                    0,
                    1)
                //// <r4rs section="6.10.1">(open-output-file <filename>)</r4rs>
                .DefinePrimitive("open-output-file", (parent, args) => EvaluateCallWithOutputFile.OpenOutputFile(List.First(args)), 1)
                //// <r4rs section="6.10.1">(output-port? <obj>)</r4rs>
                .DefinePrimitive("output-port?", (parent, args) => SchemeBoolean.Truth(List.First(args) is OutputPort), 1)
                //// <r4rs section="6.10.3">(write <obj>)</r4rs>
                //// <r4rs section="6.10.3">(write <obj> <port>)</r4rs>
                .DefinePrimitive("write", (parent, args) => Write(List.First(args), OutPort(List.Second(args), parent.Env.Interp), true), 1, 2)
                .DefinePrimitive("p", (parent, args) => P(List.First(args)), 1, 1)
                //// <r4rs section="6.10.3">(write-char <char>)</r4rs>
                //// <r4rs section="6.10.3">(write-char> <char> <port>)</r4rs>
                .DefinePrimitive(
                   "write-char",
                   (parent, args) => Write(List.First(args), OutPort(List.Second(args), parent.Env.Interp), false),
                   1,
                   2)
                .DefinePrimitive(
                   "dump-counters",
                   (parent, args) =>
                   {
                       parent.Env.Interp.DumpCounters();
                       return null;
                   },
                   0)
                .DefinePrimitive(
                   "reset-counters", 
                   (parent, args) =>
                   { 
                           parent.Env.Interp.ResetCounters();
                           return null;
                   }, 
                   0);
        }

        /// <summary>
        /// Write an object into an output port.
        /// </summary>
        /// <param name="x">The object to write.</param>
        /// <param name="port">The output port.</param>
        /// <param name="quoted">Whether to quote strings and chars.</param>
        /// <returns>The object that was output.</returns>
        public static object Write(object x, OutputPort port, bool quoted)
        {
            port.Print(SchemeString.AsString(x, quoted));
            port.Flush();
            return x;
        }

        /// <summary>
        /// Close the output port.
        /// </summary>
        public void Close()
        {
            this.outp.Close();
        }

        /// <summary>
        /// Flush the output port.
        /// </summary>
        public void Flush()
        {
            this.outp.Flush();
        }

        /// <summary>
        /// Print the object on the output port.
        /// Calls ToString to convert the object to a string.
        /// </summary>
        /// <param name="x">The object to print.</param>
        public void Print(object x)
        {
            this.outp.Write(x);
        }

        /// <summary>
        /// Print a newline to the output port.
        /// </summary>
        public void Println()
        {
            this.outp.WriteLine();
        }

        /// <summary>
        /// Print the object on the console.
        /// </summary>
        /// <param name="x">The object to print.</param>
        /// <returns>The object printed.</returns>
        private static object P(object x)
        {
            Console.Out.WriteLine(SchemeString.AsString(x));
            return x;
        }

        /// <summary>
        /// Print the object on the console along with a message.
        /// </summary>
        /// <param name="msg">The message.</param>
        /// <param name="x">The object to print.</param>
        /// <returns>The object printed.</returns>
        private static object P(string msg, object x)
        {
            Console.Out.WriteLine(msg + ": " + SchemeString.AsString(x));
            return x;
        }
    }
}