using System;
using SimpleScheme;

class Clr
{
    static void Main()
    {
        IInterpreter interp = Interpreter.New(new[] { "clr.ss" });
        interp.ReadEval(@"(define obj (new ""TestClass,Clr""))");
        interp.ReadEval("(SetAttr obj 'test)");
        Console.WriteLine(interp.ReadEvalPrint("(GetAttr obj)"));  // --> test
        Console.WriteLine(interp.ReadEvalPrint("(GetIndex obj 7)"));  // --> 0
        interp.ReadEvalPrint("(SetIndex obj 4 8)");
        Console.WriteLine(interp.ReadEvalPrint("(GetIndex obj 9)"));  // --> 8
        Console.WriteLine(interp.ReadEvalPrint("(StaticMethod 'st)"));  // --> Static Method: st
        Console.WriteLine(interp.ReadEvalPrint("(MemberMethod obj 5)"));  // --> Member Method: 5
        Console.ReadLine();
    }
}
