// <copyright file="InterpreterState.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
/// Illustrates the persistence of state in one interpreter instance.
/// </summary>
public class InterpreterState
{
    /// <summary>
    /// Successive calls to Eval in a single interpreter instance have access to the
    ///   global state that has been established by prior calls.
    /// Define a function in the global environment, and then use it in a separate call.
    /// </summary>
    public static void Main()
    {
        IInterpreter interp = Interpreter.New();
        interp.EvalStr("(define (double x) (* 2 x))");
        var res = interp.EvalStr("(double 5)");
        Console.WriteLine(interp.Print(res));   // ==> 10
        Console.ReadLine();
    }
}
