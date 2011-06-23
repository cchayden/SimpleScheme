// <copyright file="MainProgram.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Repl
{
    using SimpleScheme;

    /// <summary>
    /// The main just starts a REPL loop.
    /// </summary>
    public class MainProgram
    {
        /// <summary>
        /// Start a scheme interpreter.
        /// Pass it some files to read.
        /// Then go into a read/eval/print loop.
        /// </summary>
        /// <param name="args">These are files to read initially.</param>
        public static void Main(string[] args)
        {
            new Interpreter(args).ReadEvalWriteLoop();
        }
    }

    public class TestClass
    {
        public static string TestMethod1() { return "test 1"; }
        public string TestMethod2() { return "test 2"; }
    }
}
