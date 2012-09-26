// <copyright file="BasicTests.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
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
            Interlocked.Increment(ref sleepCounter);
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
            this.Run("<undefined>", "do", "((lambda (fun)(fun (list 0))) do)");

            this.Run("True", "if", "(procedure? if)");
            this.Run("<undefined>", "if", "((lambda (fun)(fun 0)) if)");

            this.Run("True", "let", "(procedure? let)");
            this.Run("<undefined>", "let", "((lambda (fun)(fun (list 0))) let)");

            this.Run("True", "let*", "(procedure? let*)");
            this.Run("<undefined>", "let*", "((lambda (fun)(fun (list 0))) let*)");

            this.Run("True", "letrec", "(procedure? letrec)");
            this.Run("<undefined>", "letrec", "((lambda (fun)(fun (list 0))) letrec)");

            this.Run("True", "lambda", "(procedure? lambda)");
            this.Run("(lambda (0) )", "lambda", "((lambda (fun)(fun (list 0))) lambda)");

            this.Run("True", "macro", "(procedure? macro)");
            this.Run("(macro (0) )", "lambda", "((lambda (fun)(fun (list 0))) macro)");

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
                @"(begin
                    (define return 0)
                    (define test (lambda ()
                         (+ 1 (call/cc (lambda (cont) (set! return cont) 99)))))
                    (test))"
            );
            this.Run("23", "call/cc return", "(return 22)");
            this.Run("34", "call/cc return", "(return 33)");

            this.Run("<undefined>", "call/cc seq", @"
              (begin
                (define return 0)
                (define count 0)
                (define x '(1 2 3 4 5 6 7 8))
                (define show 
                  (lambda (x) 
                    (set! count (+ 1 count))
                    (if (= x 4)
                         (call/cc (lambda (cont) (set! return cont) 4.5))
                    x))))
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
              (begin
                (define str-equals (method ""System.String"" ""Equals"" ""string"" ""string""))
                (str-equals 'xxx 'xxx))
            ");

            this.Run("False", "sync string equals", @"
              (begin
                (define str-equals (method ""System.String"" ""Equals"" ""string"" ""string""))
                (str-equals 'xxx 'yyy))
            ");

            // member method -- no arg
            this.Run("xxx", "sync trim", @"
              (begin
                (define trim (method ""System.String"" ""Trim""))
                (trim (string->symbol ""   xxx   "")))
            ");

            // member method with arg
            this.Run("xxx", "sync trim chars", @"
              (begin
                (define trim-chars (method ""System.String"" ""Trim"" ""char[]""))
                (trim-chars 'yyyxxxyyy ""y""))
            ");

            // member method with symbol arg
            this.Run("3", "sync indexOf symbol", @"
              (begin
                (define index-of (method ""System.String"" ""IndexOf"" ""string""))
                (index-of 'yyyxxxyyy (string->symbol ""x"")))
            ");

            // property get
            this.Run("3", "sync length", @"
              (begin
                (define str-length (property-get ""System.String"" ""Length""))
                (str-length 'xxx))
            ");

            // new
            this.Run("System.Collections.ArrayList", "new array-list", @"
              (begin
                (define array-list (new ""System.Collections.ArrayList""))
                array-list)
            ");

            // property get
            this.Run("0", "capacity array-list", @"
              (begin
                (define array-list (new ""System.Collections.ArrayList""))
                (define capacity (property-get ""System.Collections.ArrayList"" ""Capacity""))
                (capacity array-list))
            ");

            // property-set
            this.Run("3", "capacity array-list", @"
              (begin
                (define array-list (new ""System.Collections.ArrayList""))
                (define capacity (property-get ""System.Collections.ArrayList"" ""Capacity""))
                (define capacity-set! (property-set ""System.Collections.ArrayList"" ""Capacity"" ""int""))
                (capacity-set! array-list 3)
                (capacity array-list))
            ");

            // index get
            this.Run("1", "item array-list", @"
              (begin
                (define array-list (new ""System.Collections.ArrayList""))
                (define add (method ""System.Collections.ArrayList"" ""Add"" ""object""))
                (define item (index-get ""System.Collections.ArrayList"" ""int""))
                (define item-set! (index-set ""System.Collections.ArrayList"" ""int"" ""object"" ))
                (add array-list 1)
                (item array-list 0))
            ");

            // index set
            this.Run("3", "item array-list", @"
              (begin
                (define array-list (new ""System.Collections.ArrayList""))
                (define add (method ""System.Collections.ArrayList"" ""Add"" ""object""))
                (define item (index-get ""System.Collections.ArrayList"" ""int""))
                (define item-set! (index-set ""System.Collections.ArrayList"" ""int"" ""object"" ))
                (add array-list 1)
                (item-set! array-list 0 3)
                (item array-list 0))
            ");
        }

        /// <summary>
        /// A test for synchronous CLR procedures
        /// </summary>
        [TestMethod]
        public void CtorClrTest()
        {
            // define constructor
            this.Run("<clr-constructor>", "define string constructor", @"
                (begin
                  (define str-ctor (constructor ""string"" ""char[]""))
                  str-ctor)
            ");

            // use constructor
            this.Run("xxx", "construct string", @"
                (begin
                  (define str-ctor (constructor ""string"" ""char[]""))
                  (str-ctor ""xxx""))
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
               @"(begin
                  (define delay (method ""Tests.BasicTests,Tests"" ""TestSleep"" ""int""))
                  (define counter 0)
                  (define (delay-count x) (delay x) (set! counter (+ 1 counter)))
                  (delay-count 10)
                counter)
            ");
            Assert.AreEqual(1, sleepCounter, "sync");

            // async sleep
            sleepCounter = 0;
            this.Run("SimpleScheme.SuspendedEvaluator", "async sleep",
               @"(begin
                  (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                  (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                  (async-sleep (create-async) 10))
            ");
            Assert.AreEqual(0, sleepCounter, "async before");
            Thread.Sleep(20);
            Assert.AreEqual(1, sleepCounter, "async after");
        }

        /// <summary>
        /// A test for async interface to interpreter
        /// </summary>
        [TestMethod]
        public void BeginEndTest()
        {
            // async sleep
            sleepCounter = 0;
            this.RunAsync("10", "begin/end async sleep",
               @"(begin
                  (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                  (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                  (async-sleep (create-async) 10))
            ");
            Assert.AreEqual(1, sleepCounter, "begin/end async after");
        }

        /// <summary>
        /// A test for parallel primitive
        /// </summary>
        [TestMethod]
        public void ParallelTest()
        {
            this.Run("(1)", "parallel simple", "(parallel 1)");

            // make sure non-async calls work sequentially);
            this.Run("3", "parallel sequential",
               @"(begin
                  (define result 0)
                  (parallel (if (= result 0) (set! result 1))
                            (if (= result 1) (set! result 2))
                            (if (= result 2) (set! result 3)))
                  result)
            ");

            // all three sleep simultaneously
            // immediately after, verify no sleep is completed
            // then wait and verify that all have completed
            sleepCounter = 0;
            this.Run("SimpleScheme.SuspendedEvaluator", "parallel concurrent",
               @"(begin
                  (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                  (define sleep-caller (create-async))
                  (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                  (define (sleep duration) (async-sleep sleep-caller duration))
                  (define count 0)
                  (parallel (begin (sleep 100) (increment! count))
                            (begin (sleep 100) (increment! count))
                            (begin (sleep 100) (increment! count))))
            ");
            Assert.AreEqual(0, sleepCounter);
            this.Run("0", "parallel concurrent", "count");
            Thread.Sleep(120);
            Assert.AreEqual(3, sleepCounter);
            this.Run("3", "parallel concurrent", "count");

            // test parallel expr halts properly
            // first parallel expr sleeps asynchronously
            // second delays for a while
            // first completes its sleep
            // make sure it does not continue execution of the parallel statement
            // if it does, it would set here to #t
            // it needs to halt after the parallel sleep, not continue
            sleepCounter = 0;
            this.Run("notyet", "parallel continue",
               @"(begin
                  (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                  (define sleep-caller (create-async))
                  (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                  (define (sleep duration) (async-sleep sleep-caller duration))
                  (define delay (method ""Tests.BasicTests,Tests"" ""TestSleep"" ""int""))
                  (define here #f)
                  (define result 0)
                  (parallel (begin (sleep 100))
                            (begin (delay 200) (set! result (if here 'already 'notyet)))
                            (begin (set! here #t)))
                  result)
            ");
            Assert.AreEqual(2, sleepCounter, "parallel continue ");

            // verify that all exprs in a parallel statement continue after the async expr is finished
            sleepCounter = 0;
            this.Run("SimpleScheme.SuspendedEvaluator", "parallel continue all 1",
               @"(begin
                  (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                  (define sleep-caller (create-async))
                  (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                  (define (sleep duration) (async-sleep sleep-caller duration))
                  (define delay (method ""Tests.BasicTests,Tests"" ""TestSleep"" ""int""))

                  (define res 0)
                  (begin
                    (define res1 #f)
                    (define res2 #f)
                    (define res3 #f)
                    (parallel (begin (sleep 100) (set! res1 #t))
                              (begin (sleep 100) (set! res2 #t))
                              (begin (sleep 100) (set! res3 #t)))
                    (set! res (list res1 res2 res3))))
            ");
            Assert.AreEqual(0, sleepCounter, "parallel continue all 1");
            this.Run("0", "parallel continue all 2", "res");
            Thread.Sleep(150);
            this.Run("(#t #t #t)", "parallel continue all 2", "res");
            Assert.AreEqual(3, sleepCounter, "parallel continue all 3");

            // verify that all exprs in a parallel statement continue after the async expr is finished
            sleepCounter = 0;
            this.Run("SimpleScheme.SuspendedEvaluator", "parallel continue all 1",
               @"(begin
                  (define res 0)
                  (begin
                    (define res1 #f)
                    (define res2 #f)
                    (define res3 #f)
                    (define res4 #f)
                    (define res5 #f)
                    (parallel (begin (sleep 100) (set! res1 #t))
                              (begin (sleep 100) (set! res2 #t))
                              (begin (sleep 100) (set! res3 #t)
                                     (sleep 100) (set! res4 #t)
                                     (sleep 100) (set! res5 #t)))
                    (set! res (list res1 res2 res3 res4 res5))))
            ");
            Assert.AreEqual(0, sleepCounter, "parallel continue all 1");
            this.Run("0", "parallel continue all 2", "res");
            Thread.Sleep(350);
            this.Run("(#t #t #t #t #t)", "parallel continue all 2", "res");
            Assert.AreEqual(5, sleepCounter, "parallel continue all 3");

            // verify parallel return value
            sleepCounter = 0;
            this.Run("SimpleScheme.SuspendedEvaluator", "parallel return value",
               @"
                (begin
                  (define res 0)
                  (set! res (parallel (begin (sleep 100) 1)
                                      (begin (sleep 100) 2)
                                      (begin (sleep 100) 3))))
            ");
            this.Run("0", "parallel return value", "res");
            Thread.Sleep(150);
            this.Run("False", "parallel return value", 
                @"(null? (and 
                            (= (length res) 3) 
                            (member 1 res) 
                            (member 2 res) 
                            (member 3 res)))");
        }

        /// <summary>
        /// A test for parallel primitive with large number of clauses
        /// </summary>
        [TestMethod]
        public void LargeParallelTest()
        {
            // verify large parallel
            sleepCounter = 0;
            this.RunAsync("500", "large parallel",
               @"(begin
                   (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                   (define sleep-caller (create-async))
                   (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                   (define (sleep duration) (async-sleep sleep-caller duration))
                   (define res 0)
                   (define (build-par n par) 
                     (if (= n 0) par
                       (cons (list 'begin '(sleep 100) n)
                           (build-par (- n 1) par))))
                   (define exp (cons 'parallel (build-par 500 '())))
                   (length (eval exp)))
            ");
        }

        /// <summary>
        /// A test for parallel primitive with call/cc
        /// </summary>
        [TestMethod]
        public void ParallelContinuationTest()
        {
            // all three sleep simultaneously
            // one creates continuation
            // verify continuation does not hang and executes the right number of times in each leg
            sleepCounter = 0;
            this.Run("SimpleScheme.SuspendedEvaluator", "parallel continuation",
               @"(begin
                  (define create-async (method ""Tests.BasicTests,Tests"" ""CreateAsync""))
                  (define sleep-caller (create-async))
                  (define async-sleep (method-async ""Tests.BasicTests+TestSleepCaller,Tests"" ""Invoke"" ""int""))
                  (define (sleep duration) (async-sleep sleep-caller duration))
                  (define count1 0)
                  (define count2 0)
                  (define count3 0)
                  (define return 0)
                  (define done 0)
                  (begin
                    (parallel (begin (sleep 100) (increment! count1))
                              (begin (call/cc (lambda (cont) (set! return cont) 0)) (sleep 100) (increment! count2))
                              (begin (sleep 100) (increment! count3)))
                    (increment! done)))
            ");
            Assert.AreEqual(0, sleepCounter);
            this.Run("(0 0 0 0)", "parallel continuation", "(list count1 count2 count3 done)");
            Thread.Sleep(120);
            Assert.AreEqual(3, sleepCounter);
            this.Run("(1 1 1 1)", "parallel continuation", "(list count1 count2 count3 done)");
            this.Run("SimpleScheme.SuspendedEvaluator", "parallel continuation", "(return 0)");
            Thread.Sleep(120);
            this.Run("(1 2 2 2)", "parallel continuation", "(list count1 count2 count3 done)");
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
            string actual = res != EmptyList.New() ? res.ToString() : "'()";
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
            return this.interpreter.EvalStr(str);
        }

        /// <summary>
        /// Run a test asynchronously and check the result.
        /// </summary>
        /// <param name="expected">The expected result.</param>
        /// <param name="label">The label to display.</param>
        /// <param name="expr">The expression to evaluate.</param>
        private void RunAsync(string expected, string label, string expr)
        {
            Obj res = this.ReadAndEvaluateAsync(expr);
            string actual = res != EmptyList.New() ? res.ToString() : "'()";
            Console.WriteLine("({0} {1}) ==> {2}", label, expected, actual);
            Assert.AreEqual(expected, actual, "Failed " + this.section);
        }

        /// <summary>
        /// Read a string and evaluate it asynchronously.
        /// </summary>
        /// <param name="str">The string to read.</param>
        /// <returns>The value of the last expression.</returns>
        private Obj ReadAndEvaluateAsync(string str)
        {
            Obj expr = this.interpreter.Read(str);
            IAsyncResult res =  this.interpreter.BeginEval(expr, null, null);
            res.AsyncWaitHandle.WaitOne();
            return this.interpreter.EndEval(res);
        }
    }
}
