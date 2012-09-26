// <copyright file="Parallel.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
using System;
using System.Threading;
using SimpleScheme;

/// <summary>
/// Illustrate using BeginEval/EndEval to evaluate async expression containing a parallel primitive.
/// </summary>
public class Parallel
{
    /// <summary>
    /// An interpreter.
    /// </summary>
    private static IInterpreter interp;

    /// <summary>
    /// Delegate for DoSleep, needed for the async call.
    /// </summary>
    /// <param name="interval">The sleep duration.</param>
    /// <returns>Delegate for sleep.</returns>
    public delegate int SleepCaller(int interval);

    /// <summary>
    /// Sleep for a given interval.
    /// </summary>
    /// <param name="interval">Duration of sleep (milliseconds).</param>
    /// <returns>The interval.</returns>
    public static int DoSleep(int interval)
    {
        Thread.Sleep(interval);
        return interval;
    }

    /// <summary>
    /// Create a delegate, for calling asynchronously.
    /// </summary>
    /// <returns>DoSleep delegate.</returns>
    public static SleepCaller CreateAsyncDoSleep()
    {
        return DoSleep;
    }

    /// <summary>
    /// Parallel.ss contains a "test" function that evaluates three expressions in parallel, using the "parallel" primitive.
    /// Each one sleeps asynchronously 100 milliseconds before finishing.
    /// So at first the evaluation is not completed.  
    /// After the wait, they all should be completed.  
    /// The result of the test function is a list of the completion results of the three evaluations.
    /// This illustrates retrieving the async result after completion.
    /// </summary>
    public static void Main()
    {
        interp = Interpreter.New(new[] { "parallel.ss" });

        var startTime = DateTime.Now;
        IAsyncResult res = interp.BeginEval(interp.Read("(test)"), null, null);
        Console.WriteLine("Completed: {0}", res.IsCompleted);
        res.AsyncWaitHandle.WaitOne();
        Console.WriteLine("Completed: {0} in {1} ms", res.IsCompleted, (DateTime.Now - startTime).Milliseconds);
        Console.WriteLine("Result: {0}", ((AsyncResult<ISchemeObject>)res).Result);
        Console.ReadLine();
    }
}
