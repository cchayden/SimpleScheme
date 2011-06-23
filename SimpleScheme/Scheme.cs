﻿// <copyright file="Scheme.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.IO;

    /// <summary>
    /// The scheme interpreter instance.
    /// Each one of these is a complete interpreter, independent of others.
    /// </summary>
    public sealed partial class Scheme : SchemeUtils
    {
        /// <summary>
        /// The input port for the interpreter.
        /// </summary>
        private readonly InputPort input = new InputPort(Console.In);

        /// <summary>
        /// The output port for the interpreter.
        /// </summary>
        private readonly PrintWriter output = new PrintWriter(Console.Out);

        /// <summary>
        /// The interpreter global environment.
        /// </summary>
        private readonly Environment globalEnvironment = new Environment();

        /// <summary>
        /// Initializes a new instance of the Scheme class.
        /// Create an interpreter and install the primitives into the global environment.
        /// Then read a list of files.
        /// </summary>
        /// <param name="files">The files to read.</param>
        public Scheme(IEnumerable<string> files)
        {
            Primitive.InstallPrimitives(this.GlobalEnvironment);
            try
            {
                // TODO isn't there overlap between the installed primitives and the ones read in?
                this.Load(SchemePrimitives.Code);
                foreach (string file in files)
                {
                    this.Load(file);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Caught exception {0}", ex.Message);
            }
        }

        /// <summary>
        /// Gets the input port.
        /// </summary>
        internal InputPort Input
        {
            get { return this.input; }
        }

        /// <summary>
        /// Gets the output port.
        /// </summary>
        internal PrintWriter Output
        {
            get { return this.output; }
        }

        /// <summary>
        /// Gets the global environment for the interpreter.
        /// </summary>
        internal Environment GlobalEnvironment
        {
            get { return this.globalEnvironment; }
        }

        /// <summary>
        /// Evaluate an expression (expressed as a list) in the global environment.
        /// </summary>
        /// <param name="x">The expression to evaluate.</param>
        /// <returns>The result of the evaluation.</returns>
        public object Eval(object x)
        {
            return this.Eval(x, this.GlobalEnvironment);
        }

        /// <summary>
        /// Load a file.  
        /// Open the file and read it.
        /// Evaluate whatever it contains.
        /// </summary>
        /// <param name="fileName">The filename.</param>
        /// <returns>The result of evaluating the file contents.</returns>
        public object Load(object fileName)
        {
            string name = Stringify(fileName, false);
            try
            {
                return this.Load(
                    new InputPort(new FileStream(name, FileMode.Open, FileAccess.Read)));
            }
            catch (IOException)
            {
                return Error("can't load " + name);
            }
        }

        /// <summary>
        /// Read from the input port and evaluate whatever is there.
        /// </summary>
        /// <param name="inp">The input port.</param>
        /// <returns>True always.</returns>
        public object Load(InputPort inp)
        {
            while (true)
            {
                object x;
                if (InputPort.IsEOF(x = inp.Read()))
                {
                    inp.Close();
                    return True;
                }

                this.Eval(x);
            }
        }

        /// <summary>
        /// Read from a string and evaluate.
        /// </summary>
        /// <param name="str">The string to read and evaluate.</param>
        /// <returns>The result of the evaluation</returns>
        public object Load(string str)
        {
            using (StringReader reader = new StringReader(str))
            {
                return this.Load(new InputPort(reader));
            }
        }

        /// <summary>
        /// Read from an input port, evaluate in the global environment, and print the result.
        /// Catch and discard exceptions.
        /// </summary>
        public void ReadEvalWriteLoop()
        {
            while (true)
            {
                try
                {
                    object x;
                    this.Output.Print("> ");
                    this.Output.Flush();
                    if (InputPort.IsEOF(x = this.Input.Read()))
                    {
                        return;
                    }

                    Write(this.Eval(x), this.Output, true);
                    this.Output.Println();
                    this.Output.Flush();
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Caught exception {0}", ex.Message);
                }
            }
        }
    }
}