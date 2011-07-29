using System;
using SimpleScheme;
class BareEvaluation
{
    static void Main()
    {
        Console.WriteLine(Interpreter.New()
            .ReadEval("(let ((x 2)) (+ x 3))"));
        Console.ReadLine();
    }
}
