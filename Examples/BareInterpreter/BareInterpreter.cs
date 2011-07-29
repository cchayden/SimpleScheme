// This is the simplest example of an application embedding the Simple Scheme interpreter.
using SimpleScheme;
class BareInterpreter
{
    static void Main()
    {
        Interpreter.New()
            .ReadEvalPrintLoop();
    }
}
