
// This is the simplest example of an application embedding the Simple Scheme interpreter.
namespace BareInterpreter
{
    using SimpleScheme;
    class Program
    {
        static void Main(string[] args)
        {
            new Interpreter().ReadEvalPrintLoop();
        }
    }
}
