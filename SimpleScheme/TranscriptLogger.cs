// <copyright file="TranscriptLogger.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using Obj = System.Object;

    /// <summary>
    /// Writes input and output to a transcript.
    /// </summary>
    public class TranscriptLogger
    {
        /// <summary>
        /// The interpreter using the logger.  This is used to determine the current input
        ///   and output ports.
        /// </summary>
        private readonly Interpreter interp;

        /// <summary>
        /// Make the transcript to this TextWriter.
        /// </summary>
        private TextWriter transcriptWriter;

        /// <summary>
        /// Initializes a new instance of the TranscriptLogger class.
        /// </summary>
        /// <param name="interp">The interpreter using the logger.</param>
        internal TranscriptLogger(Interpreter interp)
        {
            this.interp = interp;
        }

        /// <summary>
        /// Turn on transcripts, writing to the given file.
        /// </summary>
        /// <param name="fileName">The file to write to.</param>
        public void TranscriptOn(Obj fileName)
        {
            if (this.transcriptWriter != null)
            {
                this.transcriptWriter.Close();
            }

            string name = Printer.AsString(fileName, false);
            this.transcriptWriter = new StreamWriter(new FileStream(name, FileMode.Create, FileAccess.Write));
        }

        /// <summary>
        /// Turn off transcripts, and close the file.
        /// </summary>
        public void TranscriptOff()
        {
            if (this.transcriptWriter != null)
            {
                this.transcriptWriter.Close();
                this.transcriptWriter = null;
            }
        }

        /// <summary>
        /// Log input to the transcript file.
        /// Do this only if the transcript is on, and the port is the current input port.
        /// </summary>
        /// <param name="str">The input to log.</param>
        /// <param name="port">The port that it came from.</param>
        internal void LogInput(string str, InputPort port)
        {
            if (this.transcriptWriter == null || port != this.interp.CurrentInputPort)
            {
                return;
            }

            this.transcriptWriter.Write(str);
        }

        /// <summary>
        /// Log output to the transcript file.
        /// Do this only if the transcript is on, and the port is the current output port.
        /// </summary>
        /// <param name="str">The output to log.</param>
        /// <param name="port">The port that it was written to.</param>
        internal void LogOutput(string str, OutputPort port)
        {
            if (this.transcriptWriter == null || port != this.interp.CurrentOutputPort)
            {
                return;
            }

            this.transcriptWriter.Write(str);
        }

        /// <summary>
        /// Log output to the transcript file, with newline.
        /// Do this only if the transcript is on, and the port is the current output port.
        /// </summary>
        /// <param name="str">The output to log.</param>
        /// <param name="port">The port that it was written to.</param>
        internal void LogOutputLine(string str, OutputPort port)
        {
            if (this.transcriptWriter == null || port != this.interp.CurrentOutputPort)
            {
                return;
            }

            this.transcriptWriter.WriteLine(str);
        }
    }
}
