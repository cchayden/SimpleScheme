// This is the simplest example of an application embedding the Simple Scheme interpreter.
using SimpleScheme;
class Program
{
    static void Main()
    {
        new Interpreter()
            .ReadEvalPrintLoop();
    }
}
