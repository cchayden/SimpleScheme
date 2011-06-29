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
            new MainProgram().Run4(args);
        }

        /// <summary>
        /// Start a scheme interpreter.
        /// Pass it some files to read.
        /// Then go into a read/eval/print loop.
        /// </summary>
        /// <param name="args">Files to read.</param>
        private void Run1(IEnumerable<string> args)
        {
            Interpreter.New(args)
                .ReadEvalPrintLoop();
        }

        /// <summary>
        /// Start a scheme interpreter.
        /// Demonstrate supplying primitive environment explicitly.
        /// </summary>
        /// <param name="args">Files to read.</param>
        private void Run2(IEnumerable<string> args)
        {
            IEnvironment primEnvironment = Environment.NewPrimitive();
            primEnvironment.InstallPrimitives();
            Interpreter.New(true, primEnvironment, args, Console.In, Console.Out)
                .ReadEvalPrintLoop();
        }

        /// <summary>
        /// Get result from REPL
        /// </summary>
        /// <param name="args">Files to read.</param>
        private void Run3(IEnumerable<string> args)
        {
            IInterpreter interp = Interpreter.New(args);
            Obj res = interp.ReadEvalPrintLoop();
            Console.WriteLine(res);
        }

        /// <summary>
        /// Example of a top level definition, befor the file is loaded.
        /// </summary>
        /// <param name="cmdlineArgs">Param not used.</param>
        private void Run4(IEnumerable<string> cmdlineArgs)
        {
            IInterpreter interp = Interpreter.New();

            // define a variable in the global environment
            interp.GlobalEnv.Define("x", 10);

            // define a primitive in the global environment
            interp.GlobalEnv.DefinePrim("plus-one", (args, caller) => Number.Num(ListPrimitives.First(args)) + 1, 1);

            // load a program stored in a string
            interp.LoadString("(p (plus-one x))");

            // evaluate a program stored in a string
            Obj res = interp.EvalString("(plus-one x)");
            Console.WriteLine(res);
            res = interp.ReadEvalPrintLoop();
            Console.WriteLine(res);
        }
    }
}
