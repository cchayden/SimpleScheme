// <copyright file="MainProgram.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Repl
{
    using System;
    using System.Collections.Generic;
    using SimpleScheme;
    using Environment = SimpleScheme.Environment;
    using Obj = System.Object;

    // TODO checklist
    // TODO check code coverage -- fill gaps
    // TODO      async, sync, timed, ...
    // TODO create a perf test case, track over time
    // TODO Examples using various .NET subsystems -- see ruby book for ideas
    // TODO Use the DLR to add a hosting environment?

    /// <summary>
    /// The main just starts a REPL loop.
    /// </summary>
    public class MainProgram
    {
        /// <summary>
        /// Run the REPL.
        /// </summary>
        /// <param name="args">These are files to read initially.</param>
        public static void Main(string[] args)
        {
            new MainProgram().Run3(args);
        }

        /// <summary>
        /// Start a scheme interpreter.
        /// Pass it some files to read.
        /// Then go into a read/eval/print loop.
        /// </summary>
        /// <param name="args">Files to read.</param>
        private void Run1(IEnumerable<string> args)
        {
            new Interpreter(args)
                .ReadEvalPrintLoop();
        }

        /// <summary>
        /// Start a scheme interpreter.
        /// Demonstrate supplying primitive environment explicitly.
        /// </summary>
        /// <param name="args">Files to read.</param>
        private void Run2(IEnumerable<string> args)
        {
            Environment primEnvironment = Environment.NewPrimitive();
            Environment.InstallPrimitives(primEnvironment);
            new Interpreter(true, primEnvironment, args, Console.In, Console.Out)
                .ReadEvalPrintLoop();
        }

        /// <summary>
        /// Get result from REPL
        /// </summary>
        /// <param name="args">Files to read.</param>
        private void Run3(IEnumerable<string> args)
        {
            var interp = new Interpreter(args);
            Obj res = interp.ReadEvalPrintLoop();
            Console.WriteLine(res);
        }
    }
}
