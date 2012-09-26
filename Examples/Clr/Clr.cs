// <copyright file="Clr.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using SimpleScheme;

/// <summary>
/// Illustrate calling methods defined in a custom class.
/// </summary>
public class Clr
{
    /// <summary>
    /// Read a file that defines procedures that link to properties and methods in our test class.
    /// Then call them one by one.
    /// Next, call a scheme function that makes the same calls.
    /// </summary>
    public static void Main()
    {
        IInterpreter interp = Interpreter.New(new[] { "clr.ss" });
        interp.EvalStr(@"(define test (new ""TestClass,Clr""))");
        interp.EvalStr("(SetAttr test 'attribute)");
        Console.WriteLine(interp.ReadEvalPrint("(GetAttr test)"));     // --> attribute
        Console.WriteLine(interp.ReadEvalPrint("(GetIndex test 7)"));  // --> 0
        interp.ReadEvalPrint("(SetIndex test 4 8)");
        Console.WriteLine(interp.ReadEvalPrint("(GetIndex test 9)"));  // --> 8
        Console.WriteLine(interp.ReadEvalPrint("(AddStar 'static)"));  // --> Static Method: static*
        Console.WriteLine(interp.ReadEvalPrint("(AddOne test 5)"));    // --> Member Method: 5
        Console.WriteLine("-----");

        interp.ReadEvalPrint("(test-clr)");
        Console.ReadLine();
    }
}
