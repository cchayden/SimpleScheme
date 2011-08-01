using System;
using SimpleScheme;

class Clr
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New(new[] { "clr.ss" });
        Console.WriteLine(interp.ReadEvalPrint("(TestMethod1)"));  // --> Static Method
        Console.WriteLine(interp.ReadEvalPrint(@"(TestMethod2 (new ""TestClass,Clr""))"));  // --> Member Method
        Console.ReadLine();
    }
}
