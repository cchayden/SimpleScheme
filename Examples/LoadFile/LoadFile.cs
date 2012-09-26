// <copyright file="LoadFile.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
/// Illustrates loading a file to initialize interpreter.
/// double.ss contains: (define (double x) (* 2 x))
/// </summary>
public class LoadFile
{
    /// <summary>
    /// Loads a file containing a function definition, and then evaluates that function.
    /// </summary>
    public static void Main()
    {
        IInterpreter interp = Interpreter.New(new[] { "double.ss" });
        Console.WriteLine(interp.EvalStr("(double 5)"));
        Console.ReadLine();
    }
}
