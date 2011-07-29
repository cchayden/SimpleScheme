// Illustrate using BeginEval/EndEval to evaluate async expression.
using System;
using SimpleScheme;
class Async
{
    private static IInterpreter interp;

    static void Main()
    {
        interp = Interpreter.New(new[] { "async.ss" });
        IAsyncResult res = interp.BeginEval(interp.Read(@"(get-content ""http://microsoft.com"")"), ShowResult, null);
        Console.WriteLine("Completed: {0}", res.IsCompleted);
        Console.ReadLine();   // wait while operation completes
        Console.WriteLine("Completed: {0}", res.IsCompleted);
        Console.ReadLine();
    }

    static void ShowResult(IAsyncResult ar)
    {
        object res = interp.EndEval(ar);
        Console.WriteLine("Result: {0}", interp.Print(res));
    }
}
