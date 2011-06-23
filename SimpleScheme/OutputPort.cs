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
        /// Print the object on the console.
        /// </summary>
        /// <param name="x">The object to print.</param>
        /// <returns>The object printed.</returns>
        public static object P(object x)
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
        public static object P(string msg, object x)
        {
            Console.Out.WriteLine(msg + ": " + SchemeString.AsString(x));
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
        /// Print the object on the output port, followed by newline.
        /// Calls ToString to convert the object to a string.
        /// </summary>
        /// <param name="x">The object to print.</param>
        public void Println(object x)
        {
            this.outp.WriteLine(x);
        }
    }
}