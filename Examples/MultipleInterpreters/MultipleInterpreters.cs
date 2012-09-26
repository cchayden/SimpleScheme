// <copyright file="MultipleInterpreters.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
///  Illustrates the independence of different interpreter instances.
/// </summary>
public class MultipleInterpreters
{
    /// <summary>
    /// Illustrates the independence of separate interpreter instances.
    /// A top level symbol definition in one interpreter is independent of that in another.  
    /// </summary>
    public static void Main()
    {
        IInterpreter interp1 = Interpreter.New();
        interp1.EvalStr("(define x 2)");
        IInterpreter interp2 = Interpreter.New();
        interp2.EvalStr("(define x 5)");
        Console.WriteLine(
            "Interp1: {0} Interp2: {1}", 
            interp1.ReadEvalPrint("x"), 
            interp2.ReadEvalPrint("x"));

        // ==> Interp1: 2 Interp2: 5
        Console.ReadLine();
    }
}
