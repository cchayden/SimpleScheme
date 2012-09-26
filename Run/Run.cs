// <copyright file="Run.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Run
{
    using System;

    using SimpleScheme;

    class Run
    {
        static void Main(string[] args)
        {
            Interpreter.New(args, true);
        }
    }
}
