// Illustrates loading a file to initialize interpreter.
// double.ss contains: (define (double x) (* 2 x))
using System;
using SimpleScheme;
class LoadFile
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New(new[] { "double.ss" });
        Console.WriteLine(interp.Eval(interp.Read("(double 5)")));
        Console.ReadLine();
    }
}
