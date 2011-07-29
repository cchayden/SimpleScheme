using System;
using SimpleScheme;
class Program
{
    static void Main()
    {
        Console.WriteLine(new Interpreter()
            .EvalString("(let ((x 2)) (+ x 3))"));
        Console.ReadLine();
    }
}
