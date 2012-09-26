namespace Fib
{
    using System;
    using SimpleScheme;

    class Fib
    {
        static void Main(string[] args)
        {
            Interpreter.New(new[] { "fib.ss" });
        }
    }
}
