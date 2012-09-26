// <copyright file="Perf.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
/// Measures the performance of the interpreter
/// </summary>
public class Perf
{
    /// <summary>
    /// Runs the performance tests in perf.ss.
    /// </summary>
    public static void Main()
    {
        Interpreter.New(new[] { "perf.ss" });
        Console.ReadLine();
    }
}
