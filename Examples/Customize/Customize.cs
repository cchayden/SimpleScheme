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
        interp.GlobalEnv.Define("x", 10);
        interp.GlobalEnv.Define("y", 20);

        // define a primitive in the global environment
        primEnvironment.DefinePrimitive(Symbol.New("plus-one"), (args, caller) => args.First().AsNumber() + 1, 1, Primitive.ValueType.Number);

        // evaluate a program stored in a string for its side effects
        interp.EvalStr("(p (plus-one x))");    // -> 11

        // evaluate a program stored in a string and get its result
        object res = interp.EvalStr("(plus-one y)");  // -> 21
        Console.WriteLine(res);
        Console.ReadLine();
    }
}
