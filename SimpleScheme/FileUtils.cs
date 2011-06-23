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
    }
}
