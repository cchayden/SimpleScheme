// <copyright file="PrintWriter.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;

    /// <summary>
    /// Writes to the output port.
    /// </summary>
    public class PrintWriter
    {
        /// <summary>
        /// The output port to write to.
        /// </summary>
        private readonly TextWriter outp;

        /// <summary>
        /// Initializes a new instance of the PrintWriter class.
        /// </summary>
        /// <param name="outp">The output port to use for writing.</param>
        public PrintWriter(TextWriter outp)
        {
            this.outp = outp;
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