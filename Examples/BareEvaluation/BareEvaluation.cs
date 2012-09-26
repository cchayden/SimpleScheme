// <copyright file="BareEvaluation.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
/// Show the interpreter being used to evaluate an expression.
/// </summary>
public class BareEvaluation
{
    /// <summary>
    /// Create an interpreter, and use it to evaluate an expression.
    /// </summary>
    public static void Main()
    {
        IInterpreter interp = Interpreter.New();
        Console.WriteLine(interp.Print(interp.EvalStr("(let ((x 2)) (+ x 3))")));

        // ==> 5
        Console.ReadLine();
    }
}
