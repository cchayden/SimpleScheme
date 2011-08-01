// Illustrates the persistence of state in one interpreter instance.
using System;
using SimpleScheme;
class InterpreterState
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New();
        interp.Eval(interp.Read("(define (double x) (* 2 x))"));
        object res = interp.Eval(interp.Read("(double 5)"));
        Console.WriteLine(interp.Print(res));   // ==> 10
        Console.ReadLine();
    }
}
