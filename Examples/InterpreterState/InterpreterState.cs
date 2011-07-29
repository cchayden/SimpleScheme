// Illustrates the persistence of state in one interpreter instance.
using System;
using SimpleScheme;
class InterpreterState
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New();
        interp.ReadEval("(define (double x) (* 2 x))");
        object res = interp.ReadEval("(double 5)");
        Console.WriteLine(res);
        Console.ReadLine();
    }
}
