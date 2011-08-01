// Illustrates the use of global state access.
using System;
using SimpleScheme;
class GlobalState
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New();
        interp.Eval(interp.Read("(define x 5)"));
        interp.GlobalEnv.Define("y", 10.0);
        Console.WriteLine(interp.Print(interp.GlobalEnv.Lookup("x")));   // ==> 5
        Console.WriteLine(interp.Print(interp.ReadEval("y")));   // ==> 10
        Console.ReadLine();
    }
}

