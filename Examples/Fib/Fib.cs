namespace Fib
{
    using SimpleScheme;

    class Fib
    {
        static void Main()
        {
            Interpreter.New(new[] { "fib.ss" });
        }
    }
}
