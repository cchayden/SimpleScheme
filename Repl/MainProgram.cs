// <copyright file="MainProgram.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Repl
{
    using System;
    using SimpleScheme;
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
        /// The files to load.
        /// </summary>
        private readonly string[] files;

        /// <summary>
        /// Initializes a new instance of the MainProgram class.
        /// </summary>
        /// <param name="files">The files to load.</param>
        private MainProgram(string[] files)
        {
            this.files = files;
        }

        /// <summary>
        /// Run the REPL.
        /// </summary>
        /// <param name="args">These are files to read initially.</param>
        public static void Main(string[] args)
        {
            string sel = args[0];
            string[] files = new string[args.Length - 1];
            Array.ConstrainedCopy(args, 1, files, 0, args.Length - 1);
            MainProgram main = new MainProgram(files);
            switch (sel)
            {
                case "simple":
                    main.RunSimple();
                    break;
                case "explicit":
                    main.RunExplicit();
                    break;
                case "custom":
                    main.RunCustom();
                    break;
                case "async":
                    main.RunAsync();
                    break;
                default:
                    Console.WriteLine("did not recognize first arg");
                    break;
            }
        }

        /// <summary>
        /// Start a scheme interpreter.
        /// Pass it some files to read.
        /// Then go into a read/eval/print loop.
        /// </summary>
        private void RunSimple()
        {
            Interpreter.New(this.files)
                .ReadEvalPrintLoop();
        }

        /// <summary>
        /// Start a scheme interpreter.
        /// Demonstrate supplying primitive environment explicitly.
        /// </summary>
        private void RunExplicit()
        {
            IPrimitiveEnvironment primEnvironment = PrimitiveEnvironment.New();
            Interpreter.New(true, primEnvironment, this.files, Console.In, Console.Out)
                .ReadEvalPrintLoop();
        }

        /// <summary>
        /// Example of a top level definition, befor the file is loaded.
        /// </summary>
        private void RunCustom()
        {
            IInterpreter interp = Interpreter.New();

            // define a variable in the global environment
            interp.GlobalEnv.Define("x", 10);

            // define a primitive in the global environment
            interp.PrimEnv.DefinePrim("plus-one", (args, caller) => Number.As(List.First(args)) + 1, 1);

            // load a program stored in a string
            interp.Load("(p (plus-one x))");

            // evaluate a program stored in a string
            Obj res = interp.ReadEval("(plus-one x)");
            Console.WriteLine(res);
            interp.ReadEvalPrintLoop();
        }

        /// <summary>
        /// Run asynchronously.
        /// </summary>
        private void RunAsync()
        {
            Obj res = Interpreter.New(this.files)
                .ReadEvalPrintAsync();
            Console.ReadLine();   // from the expression entered
            Console.ReadLine();   // waits here while operation completes
            Console.ReadLine();   // waits here after operation completes
        }
    }
}
