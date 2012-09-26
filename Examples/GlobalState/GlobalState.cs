// <copyright file="GlobalState.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
/// Illustrates the use of global state access.
/// </summary>
public class GlobalState
{
    /// <summary>
    /// Add definitions into the global environment using two methods: by evaluating "define" at the
    ///   top level, and by calling the GlobalEnv.Define method in the interpreter.
    /// Then fetch the values by (1) evaluating, and (2) looking up in the global environment
    /// </summary>
    public static void Main()
    {
        IInterpreter interp = Interpreter.New();
        interp.Eval(interp.Read("(define x 5)"));
        interp.GlobalEnv.Define("y", (Number)10);
        Console.WriteLine(interp.Print(interp.EvalStr("x")));            // ==> 5
        Console.WriteLine(interp.Print(interp.GlobalEnv.Lookup("x")));   // ==> 5
        Console.WriteLine(interp.Print(interp.EvalStr("y")));            // ==> 10
        Console.WriteLine(interp.Print(interp.GlobalEnv.Lookup("y")));   // ==> 10
        Console.ReadLine();
    }
}
