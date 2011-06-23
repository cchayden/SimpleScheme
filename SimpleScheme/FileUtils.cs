// <copyright file="FileUtils.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// These utilities manages files, input, and output.
    /// </summary>
    public class FileUtils : SchemeUtils
    {
        /// <summary>
        /// Convert an object (containing an input port) into an InputPort.
        /// If the given object is null, return the interpreter's input port.
        /// </summary>
        /// <param name="x">The object containing the input port.</param>
        /// <param name="interp">The interpreter with the default input port.</param>
        /// <returns>An input port.</returns>
        public static InputPort InPort(object x, Scheme interp)
        {
            try
            {
                return x == null ? interp.Input : (InputPort)x;
            }
            catch (InvalidCastException)
            {
                return InPort(Error("expected an input port, got: " + x), interp);
            }
        }

        /// <summary>
        /// Convert an object into an output port.
        /// If the object is null, then return the interpreter's outpot port.
        /// </summary>
        /// <param name="x">The object (should be an output port).</param>
        /// <param name="interp">The interpreter to use if the port is null.</param>
        /// <returns>An output port.</returns>
        public static OutputPort OutPort(object x, Scheme interp)
        {
            try
            {
                return x == null ? interp.Output : (OutputPort)x;
            }
            catch (InvalidCastException)
            {
                return OutPort(Error("expected an output port, got: " + x), interp);
            }
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
            port.Print(StringUtils.AsString(x, quoted));
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
            Console.Out.WriteLine(StringUtils.AsString(x));
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
            Console.Out.WriteLine(msg + ": " + StringUtils.AsString(x));
            return x;
        }
    }
}
