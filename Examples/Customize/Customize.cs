// <copyright file="Customize.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
/// Illustrates the use of an explicit primitive environment and shows
///   how an application can define primitives and variables.
/// </summary>
public class Customize
{
    /// <summary>
    /// Create an explicit prmitive environment and use it to create an interpreter.
    /// Define some variables in the interpreter's top level environment, and define
    ///   a primitive in the primitive environment.
    /// </summary>
    public static void Main()
    {
        IPrimitiveEnvironment primEnvironment = PrimitiveEnvironment.New();
        IInterpreter interp = Interpreter.New(primEnvironment);

        // define a variable in the global environment
        interp.GlobalEnv.Define("x", (Number)10);
        interp.GlobalEnv.Define("y", (Number)20);

        // define a primitive in the global environment
        primEnvironment.DefinePrimitive("plus-one", (args, caller) => (Number)(((Number)List.First(args)).N + 1), 1, SchemeObject.ValueType.Number);

        // evaluate a program stored in a string for its side effects
        interp.Eval("(p (plus-one x))");    // -> 11

        // evaluate a program stored in a string and get its result
        object res = interp.Eval("(plus-one y)");  // -> 21
        Console.WriteLine(res);
        Console.ReadLine();
    }
}
