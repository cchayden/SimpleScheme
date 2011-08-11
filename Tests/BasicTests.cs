// <copyright file="Basictests.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
    using System.IO;
    using System.Threading;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;
    using Obj = System.Object;

    /// <summary>
    /// This tests basic functionality.
    /// </summary>
    [TestClass]
    public class BasicTests
    {
        /// <summary>
        /// Counts the number of times the sleep function is called.
        /// </summary>
        private static int sleepCounter;

        /// <summary>
        /// A scheme interpreter, created for each test.
        /// </summary>
        private IInterpreter interpreter;

        /// <summary>
        /// The section of R4Rs begin tested.
        /// </summary>
        private string section;

        /// <summary>
        /// Delegate for TestSleep, needed for the async call.
        /// </summary>
        /// <param name="interval">The dleep duration.</param>
        /// <returns>Delegate for sleep.</returns>
        public delegate int TestSleepCaller(int interval);

        /// <summary>
        /// Gets or sets the test context which provides
        /// information about and functionality for the current test run.
        /// </summary>
        public TestContext TestContext { get; set; }

        /// <summary>
        /// Sleep for a given interval.
        /// </summary>
        /// <param name="interval">Duration of sleep (milliseconds).</param>
        /// <returns>The interval.</returns>
        public static int TestSleep(int interval)
        {
            Thread.Sleep(interval);
            sleepCounter++;
            return interval;
        }

        /// <summary>
        /// Create a delegate, for calling asynchronously.
        /// </summary>
        /// <returns>TestSleep delegate.</returns>
        public static TestSleepCaller CreateAsync()
        {
            return TestSleep;
        }

        /// <summary>
        /// Initialize each test.
        /// </summary>
        [TestInitialize]
        public void MyTestInitialize()
        {
            this.interpreter = Interpreter.New();
            sleepCounter = 0;
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
        /// A test for synchronous CLR procedures
        /// </summary>
        [TestMethod]
        public void SyncClrTest()
        {
            // static method
            this.Run("True", "sync string equals", @"
                (define str-equals (method ""System.String"" ""Equals"" ""string"" ""string""))
                (str-equals 'xxx 'xxx)
            ");

            this.Run("False", "sync string equals", @"
                (define str-equals (method ""System.String"" ""Equals"" ""string"" ""string""))
                (str-equals 'xxx 'yyy)
            ");

            // member method -- no arg
            this.Run("xxx", "sync trim", @"
                (define trim (method ""System.String"" ""Trim""))
                (trim (string->symbol ""   xxx   ""))
            ");

            // member method with arg
            this.Run("xxx", "sync trim chars", @"
                (define trim-chars (method ""System.String"" ""Trim"" ""char[]""))
                (trim-chars 'yyyxxxyyy ""y"")
            ");

            // property get
            this.Run("3", "sync length", @"
                (define str-length (property-get ""System.String"" ""Length""))
                (str-length 'xxx)
            ");

            // new
            this.Run("System.Collections.ArrayList", "new array-list", @"
                (define array-list (new ""System.Collections.ArrayList""))
                array-list
            ");

            // property get
            this.Run("0", "capacity array-list", @"
                (define array-list (new ""System.Collections.ArrayList""))
                (define capacity (property-get ""System.Collections.ArrayList"" ""Capacity""))
                (capacity array-list)
            ");

            // property-set
            this.Run("3", "capacity array-list", @"
                (define array-list (new ""System.Collections.ArrayList""))
                (define capacity (property-get ""System.Collections.ArrayList"" ""Capacity""))
                (define capacity-set! (property-set ""System.Collections.ArrayList"" ""Capacity"" ""int""))
                (capacity-set! array-list 3)
                (capacity array-list)
            ");

            // index get
            this.Run("1", "item array-list", @"
                (define array-list (new ""System.Collections.ArrayList""))
                (define add (method ""System.Collections.ArrayList"" ""Add"" ""object""))
                (define item (index-get ""System.Collections.ArrayList"" ""int""))
                (define item-set! (index-set ""System.Collections.ArrayList"" ""int"" ""object"" ))
                (add array-list 1)
                (item array-list 0)
            ");

            // index set
            this.Run("3", "item array-list", @"
                (define array-list (new ""System.Collections.ArrayList""))
                (define add (method ""System.Collections.ArrayList"" ""Add"" ""object""))
                (define item (index-get ""System.Collections.ArrayList"" ""int""))
                (define item-set! (index-set ""System.Collections.ArrayList"" ""int"" ""object"" ))
                (add array-list 1)
                (item-set! array-list 0 3)
                (item array-list 0)
            ");
        }

        /// <summary>
        /// A test for synchronous CLR procedures
        /// </summary>
        [TestMethod]
        public void CtorClrTest()
        {
            // define constructor
            this.Run("<clr constructor>", "define string constructor", @"
                (define str-ctor (constructor ""string"" ""char[]""))
                str-ctor
            ");

            // use constructor
            this.Run("xxx", "construct string", @"
                (define str-ctor (constructor ""string"" ""char[]""))
                (str-ctor ""xxx"")
            ");
      }

        /// <summary>
        /// A test for asynchronous CLR procedures
        /// </summary>
        [TestMethod]
        public void AsyncClrTest()
        {
            // sync sleep
            sleepCounter = 0;
            this.Run("1", "sync sleep",
               @"
                (define delay (method ""Tests.BasicTests,Tests"" ""TestSleep"" ""int""))
                (define counter 0)
                (define (delay-count x) (delay x) (set! counter (+ 1 counter)))
                (delay-count 10)
                counter
            ");
            Assert.AreEqual(1, sleepCounter, "sync");

            // async sleep
            sleepCounter = 0;
            this.Run("SimpleScheme.Evaluator", "async sleep",
               @"
                (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                (async-sleep (create-async) 10)
            ");
            Assert.AreEqual(0, sleepCounter, "async before");
            Thread.Sleep(20);
            Assert.AreEqual(1, sleepCounter, "async after");
        }

        /// <summary>
        /// A test for parallel primitive
        /// </summary>
        [TestMethod]
        public void ParallelClrTest()
        {
            // make sure non-async calls work sequentially
            this.Run("3", "parallel sequential",
               @"
                (define result 0)
                (parallel (if (= result 0) (set! result 1))
                          (if (= result 1) (set! result 2))
                          (if (= result 2) (set! result 3)))
                result
            ");

            // all three sleep simultaneously
            // immediately after, verify no sleep is completed
            // then wait and verify that all have completed
            sleepCounter = 0;
            this.Run("<undefined>", "async sleep basic",
               @"
                (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                (define sleep-caller (create-async))
                (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                (define (sleep duration) (async-sleep sleep-caller duration) (display 'done))
                (parallel (sleep 100) 
                          (sleep 100) 
                          (sleep 100))
            ");
            Assert.AreEqual(0, sleepCounter, "async before");
            Thread.Sleep(120);
            Assert.AreEqual(3, sleepCounter, "async after");

            // test parallel expr halts properly
            // first parallel expr sleeps asynchronously
            // second delays for a while
            // first completes its sleep
            // make sure it does not continue execution of the parallel statement
            // if it does, it would set here to #t
            // it needs to halt after the parallel sleep, not continue
            sleepCounter = 0;
            this.Run("notyet", "async sleep sequence",
               @"
                (define delay (method ""Tests.BasicTests,Tests"" ""TestSleep"" ""int""))
                (define here #f)
                (define result 0)
                (parallel (begin (sleep 100))
                          (begin (delay 200) (set! result (if here 'already 'notyet)))
                          (begin (set! here #t)))
                result
            ");
            Assert.AreEqual(2, sleepCounter, "async ");

            // verify that all exprs in a parallel statement continue after the async op is finished
            sleepCounter = 0;
            this.Run("(#f #f #f)", "async sleep continue",
               @"
                (define res1 #f)
                (define res2 #f)
                (define res3 #f)
                (parallel (begin (sleep 100) (set! res1 #t))
                          (begin (sleep 100) (set! res2 #t))
                          (begin (sleep 100) (set! res3 #t)))
                (list res1 res2 res3))
            ");
            Assert.AreEqual(0, sleepCounter, "async ");
            this.Run("(#t #t #t)", "async sleep continue",
               @"
                 (delay 120) 
                 (list res1 res2 res3)
            ");
            Assert.AreEqual(4, sleepCounter, "async ");
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
            string actual = res != EmptyList.Instance ? res.ToString() : "'()";
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
                InputPort inp = new InputPort(reader, (Interpreter)this.interpreter);
                Obj last = EmptyList.Instance;
                while (true)
                {
                    Obj x;
                    if (InputPort.IsEof(x = inp.Read()))
                    {
                        return last;
                    }

                    last = this.interpreter.Eval(x);
                }
            }
        }
    }
}
