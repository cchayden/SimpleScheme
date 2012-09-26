// <copyright file="MainProgram.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
// Most came from http://www.ccs.neu.edu/home/will/Twobit/benchmarksAboutR6.html
// See also: http://svn​.plt-schem​e.org/plt/​trunk/coll​ects/tests​/mzscheme/​benchmarks​/common/
namespace Benchmark
{
    using System;
    using System.IO;

    using SimpleScheme;

    /// <summary>
    /// The main just starts a REPL loop.
    /// </summary>
    public class MainProgram
    {
        private static double totalMsec = 0;

        public static IInterpreter RunBenchmark(TextWriter fs, string filenameBase)
        {
            string[] files = new[] { filenameBase + ".ss", "common.ss" };
            IInterpreter interp = Interpreter.New(files);
            interp.GlobalEnvironment.Define("input-filename", (SchemeString)(filenameBase + ".input"));
            var res = interp.Eval(interp.Read("(main1 input-filename)"));
            var name = List.First(res);
            var count = List.Nth(res, (Number)1);
            var msec = List.Nth(res, (Number)2);
            var mem = List.Nth(res, (Number)3);
            fs.WriteLine("{0},{1},{2},{3}", name, count, msec, mem); 
            fs.Flush();
            Console.WriteLine("name: {0} count: {1} msec: {2} mem: {3}", name, count, msec, mem);
            totalMsec += ((Number)msec).N;
            return interp;
        }

        /// <summary>
        /// Run the REPL.
        /// </summary>
        /// <param name="args">These are files to read initially.</param>
        public static void Main(string[] args)
        {
            if (args[0] == "all")
            {
                args = new[]
                    {
                        "sum", "sumfp", "tak", "takl", "ctak", "cpstak", "ntakl", "deriv", "diviter", "divrec", "fft",
                        "fib", "fibc", "fibfp", "mbrot", "scheme", "triangl"
                    };
            }

            using (FileStream fs = File.OpenWrite("bench.out"))
            {
                using (TextWriter writer = new StreamWriter(fs))
                {
                    foreach (string name in args)
                    {
                        RunBenchmark(writer, name);
                    }

                    writer.WriteLine("Total,,{0}", totalMsec / 1000.0);
                }
            }

            // Console.ReadLine();
        }
    }
}
