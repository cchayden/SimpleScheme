using System;
using SimpleScheme;

class Clr
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New(new[] { "clr.ss" });
        Console.WriteLine(interp.ReadEval("(TestMethod1)"));  // --> Static Method
        Console.WriteLine(interp.ReadEval(@"(TestMethod2 (new ""TestClass,Clr""))"));  // --> Member Method
        Console.ReadLine();
    }
}
