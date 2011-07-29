// Illustrates the independence of different interpreter instances.
using System;
using SimpleScheme;
class MultipleInterpreters
{
    static void Main()
    {
        IInterpreter interp1 = Interpreter.New();
        interp1.ReadEval("(define x 2)");
        IInterpreter interp2 = Interpreter.New();
        interp2.ReadEval("(define x 5)");
        Console.WriteLine("Interp1: {0} Interp2: {1}", interp1.ReadEval("x"), interp2.ReadEval("x"));
        Console.ReadLine();
    }
}
