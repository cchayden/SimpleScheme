// Illustrates loading a file to initialize interpreter.
// double.ss contains: (define (double x) (* 2 x))
using System;
using SimpleScheme;
class LoadFile
{
    static void Main()
    {
        Console.WriteLine(Interpreter.New(new[] { "double.ss" })
            .ReadEval("(double 5)"));
        Console.ReadLine();

    }
}
