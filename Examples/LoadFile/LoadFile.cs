// Illustrates loading a file to initialize interpreter.
// double.ss contains: (define (double x) (* 2 x))
using System;
using SimpleScheme;
class Program
{
    static void Main()
    {
        Console.WriteLine(new Interpreter(new[] { "double.ss" })
            .EvalString("(double 5)"));
        Console.ReadLine();

    }
}
