// Illustrate using BeginEval/EndEval to evaluate async expression containing a parallel primitive.
using System;
using System.Threading;
using SimpleScheme;
class Parallel
{
    private static IInterpreter interp;

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
    /// Delegate for DoSleep, needed for the async call.
    /// </summary>
    /// <param name="interval">The dleep duration.</param>
    /// <returns>Delegate for sleep.</returns>
    public delegate int SleepCaller(int interval);

    /// <summary>
    /// Create a delegate, for calling asynchronously.
    /// </summary>
    /// <returns>DoSleep delegate.</returns>
    public static SleepCaller CreateAsync()
    {
        return DoSleep;
    }

    static void Main()
    {
        interp = Interpreter.New(new[] { "parallel.ss" });

        var startTime = DateTime.Now;
        IAsyncResult res = interp.BeginEval(interp.Read("(test)"), null, null);
        Console.WriteLine("Completed: {0}", res.IsCompleted);
        res.AsyncWaitHandle.WaitOne();
        Console.WriteLine("Completed: {0} in {1} ms", res.IsCompleted, (DateTime.Now - startTime).Milliseconds);
        Console.WriteLine("Result: {0}", ((AsyncResult<object>)res).Result);
        //interp.ReadEvalPrintLoop();
        Console.ReadLine();
    }
}
