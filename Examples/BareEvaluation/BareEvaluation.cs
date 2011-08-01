using System;
using SimpleScheme;
class BareEvaluation
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New();
        Console.WriteLine(interp.Print(interp.Eval(interp.Read("(let ((x 2)) (+ x 3))"))));
        // ==> 5
        Console.ReadLine();
    }
}
