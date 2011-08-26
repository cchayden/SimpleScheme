// Measures the performance of the interpreter
using System;
using SimpleScheme;
class Perf
{
    static void Main()
    {
        Interpreter.New(new[] { "perf.ss" });
        Console.ReadLine();
    }
}
