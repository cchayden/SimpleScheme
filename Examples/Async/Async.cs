// <copyright file="Async.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using System.Threading;
using SimpleScheme;

/// <summary>
/// Illustrate using BeginEval/EndEval to evaluate async expression.
/// </summary>
public class Async
{
    /// <summary>
    /// The interpreter
    /// </summary>
    private static IInterpreter interp;

    /// <summary>
    /// Flag indicating async completion.
    /// </summary>
    private static bool isComplete;  // = false;

    /// <summary>
    /// Async.ss uses System.Net.WebRequest to (asynchronously) fetch a web page (in the function get-content).
    /// Since it is asynchronous, it has not completed when the evaluation returns.
    /// After completion, the callback ShowResult is called, which displays the page contents.
    /// This allows the main method to proceed.
    /// While it is executing, get-content displays the number of bytes in each read that it does.
    /// </summary>
    public static void Main()
    {
        interp = Interpreter.New(new[] { "async.ss" });
        IAsyncResult res = interp.BeginEval(interp.Read(@"(get-content ""http://microsoft.com"")"), ShowResult, null);
        Console.WriteLine("Completed: {0}", res.IsCompleted);
        while (isComplete == false)
        {
            Thread.Sleep(100);
        }

        Console.WriteLine("Completed: {0}", res.IsCompleted);
        Console.ReadLine();
    }

    /// <summary>
    /// Called when BeginEval completes.  Gets async result and prints it.
    /// </summary>
    /// <param name="ar">The async result.</param>
    private static void ShowResult(IAsyncResult ar)
    {
        ISchemeObject res = interp.EndEval(ar);
        Console.WriteLine("Result: {0}", interp.Print(res));
        isComplete = true;
    }
}
