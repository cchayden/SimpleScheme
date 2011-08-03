// <copyright file="Basictests.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
    using System.IO;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;
    using Obj = System.Object;

    /// <summary>
    /// This tests basic functionality.
    /// </summary>
    [TestClass]
    public class Basictests
    {
        /// <summary>
        /// A scheme interpreter, created for each test.
        /// </summary>
        private IInterpreter interpreter;

        /// <summary>
        /// The section of R4Rs begin tested.
        /// </summary>
        private string section;

        /// <summary>
        /// Gets or sets the test context which provides
        /// information about and functionality for the current test run.
        /// </summary>
        public TestContext TestContext { get; set; }

        /// <summary>
        /// Initialize each test.
        /// </summary>
        [TestInitialize]
        public void MyTestInitialize()
        {
            this.interpreter = Interpreter.New();
        }

        /// <summary>
        /// Test that all special forms are defined as procedures.
        /// Also make sure they can be evaluated when passed as a proc.
        /// </summary>
        [TestMethod]
        public void SpecialFormsTest()
        {
            this.section = "0.0";
            this.Run("True", "and", "(procedure? and)");
            this.Run("0", "and", "((lambda (fun)(fun 0)) and)");

            this.Run("True", "or", "(procedure? or)");
            this.Run("0", "or", "((lambda (fun)(fun 0)) or)");

            this.Run("True", "begin", "(procedure? begin)");
            this.Run("0", "begin", "((lambda (fun)(fun 0)) begin)");

            this.Run("True", "cond", "(procedure? cond)");
            this.Run("'()", "cond", "((lambda (fun)(fun 0)) cond)");

            this.Run("True", "define", "(procedure? define)");
            this.Run("<undefined>", "define", "((lambda (fun)(fun 'a)) define)");

            this.Run("True", "do", "(procedure? do)");
            this.Run("<undefined>", "do", "((lambda (fun)(fun 0)) do)");

            this.Run("True", "if", "(procedure? if)");
            this.Run("<undefined>", "if", "((lambda (fun)(fun 0)) if)");

            this.Run("True", "let", "(procedure? let)");
            this.Run("<undefined>", "let", "((lambda (fun)(fun 0)) let)");

            this.Run("True", "let*", "(procedure? let*)");
            this.Run("<undefined>", "let*", "((lambda (fun)(fun 0)) let*)");

            this.Run("True", "letrec", "(procedure? letrec)");
            this.Run("<undefined>", "letrec", "((lambda (fun)(fun 0)) letrec)");

            this.Run("True", "lambda", "(procedure? lambda)");
            this.Run("(lambda 0 )", "lambda", "((lambda (fun)(fun 0)) lambda)");

            this.Run("True", "macro", "(procedure? macro)");
            this.Run("(macro 0 )", "lambda", "((lambda (fun)(fun 0)) macro)");

            this.Run("True", "quote", "(procedure? quote)");
            this.Run("0", "quote", "((lambda (fun)(fun 0)) quote)");

            this.Run("True", "set!", "(procedure? set!)");
            this.Run("<undefined>", "set!", "(define x 10)((lambda (fun)(fun 'x 0)) set!)");

            this.Run("True", "time", "(procedure? time)");
            this.Run("0", "time", "(first ((lambda (fun)(fun 0)) time))");
        }

        /// <summary>
        /// A test for call/cc
        /// The R4RSTest tests do not do a good enough job.
        /// </summary>
        [TestMethod]
        public void CallccTest()
        {
            this.Run("100", "call/cc", 
                @"(define return 0)
                  (define test (lambda ()
                       (+ 1 (call/cc (lambda (cont) (set! return cont) 99)))))
                  (test)"
            );
            this.Run("23", "call/cc return", "(return 22)");
            this.Run("34", "call/cc return", "(return 33)");

            this.Run("<undefined>", "call/cc seq", @"
              (define return 0)
              (define count 0)
              (define x '(1 2 3 4 5 6 7 8))
              (define show 
                (lambda (x) 
                  (set! count (+ 1 count))
                  (if (= x 4)
                       (call/cc (lambda (cont) (set! return cont) 4.5))
                  x)))
            ");
            this.Run("(1 2 3 4.5 5 6 7 8)", "call/cc seq map", "(map show x)");
            this.Run("8", "call/cc count", "count");
            this.Run("(1 2 3 22 5 6 7 8)", "call/cc seq return", "(return 22)");
            this.Run("12", "call/cc count", "count");
            this.Run("(1 2 3 33 5 6 7 8)", "call/cc seq return", "(return 33)");
            this.Run("16", "call/cc count", "count");
        }

        /// <summary>
        /// Run a test and check the result.
        /// </summary>
        /// <param name="expected">The expected result.</param>
        /// <param name="label">The label to display.</param>
        /// <param name="expr">The expression to evaluate.</param>
        private void Run(string expected, string label, string expr)
        {
            Obj res = this.ReadAndEvaluate(expr);
            string actual = res != EmptyList_Accessor.Instance ? res.ToString() : "'()";
            Console.WriteLine("({0} {1}) ==> {2}", label, expected, actual);
            Assert.AreEqual(expected, actual, "Failed " + this.section);
        }

        /// <summary>
        /// Read a string and evaluate it.
        /// </summary>
        /// <param name="str">The string to read.</param>
        /// <returns>The value of the last expression.</returns>
        private Obj ReadAndEvaluate(string str) 
        {
            using (StringReader reader = new StringReader(str))
            {
                InputPort_Accessor inp = new InputPort_Accessor(reader, (Interpreter)this.interpreter);
                Obj last = EmptyList_Accessor.Instance;
                while (true)
                {
                    Obj x;
                    if (InputPort_Accessor.IsEof(x = inp.ReadObj()))
                    {
                        return last;
                    }

                    last = this.interpreter.Eval(x);
                }
            }
        }
    }
}
