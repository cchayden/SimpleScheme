// Illustrates the use of an explicit primitive environment and shows
//  how an application can defind primitives and variables.
using System;
using SimpleScheme;
class Customize
{
    static void Main()
    {
        IPrimitiveEnvironment primEnvironment = PrimitiveEnvironment.New();
        IInterpreter interp = Interpreter.New(primEnvironment);

        // define a variable in the global environment
        interp.GlobalEnv.Define("x", 10);
        interp.GlobalEnv.Define("y", 20);

        // define a primitive in the global environment
        primEnvironment.DefinePrimitive("plus-one", (args, caller) => Number.As(List.First(args)) + 1, 1);

        // load a program stored in a string
        interp.Load("(p (plus-one x))");    // -> 11

        // evaluate a program stored in a string
        object res = interp.ReadEval("(plus-one y)");  // -> 21
        Console.WriteLine(res);
        Console.ReadLine();
    }
}
