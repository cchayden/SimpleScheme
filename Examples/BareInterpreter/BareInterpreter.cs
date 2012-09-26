// <copyright file="BareInterpreter.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using SimpleScheme;

/// <summary>
/// This is the simplest example of an application embedding the Simple Scheme interpreter.
/// </summary>
public class BareInterpreter
{
    /// <summary>
    /// Instantiate interpreter and let it read and interpret expressions.
    /// </summary>
    public static void Main()
    {
        Interpreter.New()
            .ReadEvalPrintLoop();
    }
}
