// Illustrate using BeginEval/EndEval to evaluate async expression containing a parallel primitive.
using System;
using System.Threading;
using SimpleScheme;
class Parallel
{
    private static IInterpreter interp;

    /// <summary>
    /// Counts the number of times the sleep function is called.
    /// </summary>
    private static int sleepCounter;

    /// <summary>
    /// Delegate for TestSleep, needed for the async call.
    /// </summary>
    /// <param name="interval">The dleep duration.</param>
    /// <returns>Delegate for sleep.</returns>
    public delegate int TestSleepCaller(int interval);

    /// <summary>
    /// Sleep for a given interval.
    /// </summary>
    /// <param name="interval">Duration of sleep (milliseconds).</param>
    /// <returns>The interval.</returns>
    public static int TestSleep(int interval)
    {
        Thread.Sleep(interval);
        Interlocked.Increment(ref sleepCounter);
        return interval;
    }

    /// <summary>
    /// Create a delegate, for calling asynchronously.
    /// </summary>
    /// <returns>TestSleep delegate.</returns>
    public static TestSleepCaller CreateAsync()
    {
        return TestSleep;
    }

    static void Main()
    {
        interp = Interpreter.New(new[] { "parallel.ss" });

        IAsyncResult res = interp.BeginEval(interp.Read("(test)"), null, null);
        Console.WriteLine("Completed: {0}", res.IsCompleted);
        res.AsyncWaitHandle.WaitOne();
        Console.WriteLine("Completed: {0}", res.IsCompleted);
        Console.WriteLine("Result: {0}", ((AsyncResult<object>)res).Result);
        Console.ReadLine();
    }
}
