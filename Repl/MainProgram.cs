// <copyright file="MainProgram.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Repl
{
    using System.Collections.Generic;
    using SimpleScheme;

    // TODO checklist
    // TODO check code coverage -- fill gaps
    // TODO      async, sync, timed, ...
    // TODO create a perf test case, track over time
    // TODO Examples using various .NET subsystems -- see ruby book for ideas
    // TODO Use the DLR to add a hosting environment??

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
            new MainProgram().Run2(args);
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
                .ReadEvalWriteLoop();
        }

        /// <summary>
        /// Start a scheme interpreter.
        /// Demonstrate supplying primitive environment explicitly.
        /// </summary>
        /// <param name="args">Files to read.</param>
        private void Run2(IEnumerable<string> args)
        {
            Environment primEnvironment = new Environment()
                .InstallPrimitives();
            new Interpreter(true, primEnvironment, args)
                .ReadEvalWriteLoop();
        }
    }
}
