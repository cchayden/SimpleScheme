﻿// <copyright file="MainProgram.cs" company="Charles Hayden">
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
        /// Run the REPL.
        /// </summary>
        /// <param name="args">These are files to read initially.</param>
        public static void Main(string[] args)
        {
            Interpreter.New(args)
                .ReadEvalPrintLoop();
        }
    }
}
