// <copyright file="R4RSUnitTests.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
    using System.IO;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;

    /// <summary>
    /// This is a test class for R4RSTest and is intended
    /// to contain all R4RSTest Unit Tests
    /// </summary>
    [TestClass]
    public class R4RsUnitTests
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
        /// Test that all symbol characters are supported.
        /// </summary>
        [TestMethod]
        public void IdentifierTest()
        {
            this.section = "2.1";
            const string Test = "'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)";
            SchemeObject res = this.ReadAndEvaluate(Test);
            Assert.AreEqual(17, List.ListLength(res), "Failed " + this.section);
            Assert.AreEqual(Test.Substring(1), res.ToString(), "Failed " + this.section);
        }

        /// <summary>
        /// Test type predicates
        /// Make sure the type predicates correctly identify primitive types.
        /// </summary>
        [TestMethod]
        public void TypePredicateTest()
        {
            this.section = "3.4";
            var predicates = new[] { "boolean?", "char?",      "null?", "number?", "pair?",    "procedure?", "string?",    "symbol?", "vector?" };
            var examples1 = new[] { "#t",        "#\\a",       "'()",   "9739",    "'(test)",  "double",     @"""test""",  "'car",     "'#(a b c)" };
            var examples2 = new[] { "#f",        "#\\newline", "'()",   "-3252",   "'(t . t)", "car",        @"""""",      "'nil",     "'#()" };
            this.ReadAndEvaluate("(define double (lambda (x) (* 2 x)))");
            string[][] examples = { examples1, examples2 };
            foreach (string[] ex in examples)
            {
                for (int i = 0; i < predicates.Length; i++)
                {
                    for (int j = 0; j < examples1.Length; j++)
                    {
                        string test = string.Format("({0} {1})", predicates[i], ex[j]);
                        SchemeObject res = this.ReadAndEvaluate(test);
                        Assert.IsInstanceOfType(res, typeof(SchemeBoolean));
                        Assert.AreEqual(i == j, ((SchemeBoolean)res).Value, "Failed " + this.section + " " + test);
                    }
                }
            }
        }

        /// <summary>
        /// Test quoteing
        /// </summary>
        [TestMethod]
        public void QuoteTest()
        {
            this.section = "4.1.2";
            Assert.AreEqual("'a", this.ReadAndEvaluate("'quote (quote 'a)").ToString(), "Failed " + this.section);
            Assert.AreEqual("'a", this.ReadAndEvaluate("'quote ''a").ToString(), "Failed " + this.section);
        }

        /// <summary>
        /// Test if statements
        /// </summary>
        [TestMethod]
        public void IfTest()
        {
            this.section = "4.1.3";
            Assert.AreEqual(12.0, ((Number)this.ReadAndEvaluate("((if #f + *) 3 4)")).N, "Failed " + this.section);
        }

        /// <summary>
        /// Test lambda
        /// </summary>
        [TestMethod]
        public void LambdaTest()
        {
            this.section = "4.1.4";
            Assert.AreEqual(8.0, ((Number)this.ReadAndEvaluate("((lambda (x) (+ x x)) 4)")).N, "Failed " + this.section);
            this.ReadAndEvaluate("(define reverse-subtract (lambda (x y) (- y x)))");
            Assert.AreEqual(3.0, ((Number)this.ReadAndEvaluate("(reverse-subtract 7 10)")).N, "Failed " + this.section);
            this.ReadAndEvaluate(
                @"(define add4
                    (let ((x 4))
                         (lambda (y) (+ x y))))");

            Assert.AreEqual(10.0, ((Number)this.ReadAndEvaluate("(add4 6)")).N, "Failed 4.1.4");
            Assert.AreEqual("(3 4 5 6)", this.ReadAndEvaluate("((lambda x x) 3 4 5 6)").ToString(), "Failed " + this.section);
            Assert.AreEqual("(5 6)", this.ReadAndEvaluate("((lambda (x y . z) z) 3 4 5 6))").ToString(), "Failed " + this.section);
        }

        /// <summary>
        /// Test more if
        /// </summary>
        [TestMethod]
        public void IfMoreTest()
        {
            this.section = "4.1.5";
            this.Run("3", "define", 
                     @"(define x 2)
                       (+ x 1)");
            this.Run("5", "set!",
                      @"(set! x 4)
                        (+ x 1)");
        }

        /// <summary>
        /// Test define and set!
        /// </summary>
        [TestMethod]
        public void DefineSetTest()
        {
            this.section = "4.1.6";
            this.Run("yes", "if", "(if (> 3 2) 'yes 'no)");
            this.Run("no", "if", "(if (> 2 3) 'yes 'no)");
            this.Run("1", "if", "(if (> 3 2) (- 3 2) (+ 3 2))");
        }

        /// <summary>
        /// Test conditionals, and, and or
        /// </summary>
        [TestMethod]
        public void ConditionalsTest()
        {
            this.section = "4.2.1";
            this.Run("greater", "cond", 
                  @"(cond ((> 3 2) 'greater)
			              ((< 3 2) 'less))");
            this.Run("equal", "cond", 
                     @"(cond ((> 3 3) 'greater)
			                 ((< 3 3) 'less)
			                 (else 'equal))");
            this.Run("2", "cond", 
                    @"(cond ((assv 'b '((a 1) (b 2))) => cadr)
		                    (else #f))");
            this.Run("composite", "case", 
                     @"(case (* 2 3)
			               ((2 3 5 7) 'prime)
			               ((1 4 6 8 9) 'composite))");
            this.Run("consonant", "case", 
                  @"(case (car '(c d))
			                ((a e i o u) 'vowel)
        			        ((w y) 'semivowel)
			                (else 'consonant))");

            this.Run("#t", "and", "(and (= 2 2) (> 2 1))");
            this.Run("#f", "and", "(and (= 2 2) (< 2 1))");
            this.Run("(f g)", "and", "(and 1 2 'c '(f g))");
            this.Run("#t", "and", "(and)");
            this.Run("#t", "or", "(or (= 2 2) (> 2 1))");
            this.Run("#t", "or", "(or (= 2 2) (< 2 1))");
            this.Run("#f", "or", "(or #f #f #f)");
            this.Run("#f", "or", "(or)");
            this.Run("(b c)", "or", "(or (memq 'b '(a b c)) (+ 3 0))");
        }

        /// <summary>
        /// Test let and friends
        /// </summary>
        [TestMethod]
        public void LetTest()
        {
            this.section = "4.2.2";
            this.Run("6", "let", "(let ((x 2) (y 3)) (* x y))");
            this.Run("35", "let", "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))");
            this.Run("70", "let*", "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))");
            this.Run("#t",  "letrec", 
                          @"(letrec 
                             ((even?
    			               (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
			                 (odd?
			                   (lambda (n) (if (zero? n) #f (even? (- n 1))))))
		                   (even? 88))");
            this.Run("5", "let", 
                 @"(define x 34)
                   (let ((x 3)) 
                      (define x 5) x)");
            this.Run("34", "let", "x");

            this.Run("6", "let", "(let () (define x 6) x)");
            this.Run("34", "let", "x");
            this.Run("34", "let",  "(let ((x x)) x)");
            this.Run("7", "let*", "(let* ((x 3)) (define x 7) x)");
            this.Run("34", "let*", "x");
            this.Run("8", "let*", "(let* () (define x 8) x)");
            this.Run("34", "let*",  "x");
            this.Run("9", "letrec",  "(letrec () (define x 9) x)");
            this.Run("34", "letrec",  "x");
            this.Run("10", "letrec",  "(letrec ((x 3)) (define x 10) x)");
            this.Run("34", "letrec",  "x");
        }

        /// <summary>
        /// Test begin
        /// </summary>
        [TestMethod]
        public void BeginTest()
        {
            this.section = "4.2.3";
            Assert.AreEqual("6", this.ReadAndEvaluate(
                           @"(define x 0)
                             (begin (begin (set! x (begin (begin 5)))
		                        (begin ((begin +) (begin x) (begin (begin 1))))))").ToString(), "Failed " + this.section);
        }

        /// <summary>
        /// Test loops
        /// </summary>
        [TestMethod]
        public void LoopTest()
        {
            this.section = "4.2.4";
            this.Run("#(0 1 2 3 4)", "do", 
            @" (do ((vec (make-vector 5))
			          (i 0 (+ i 1)))
			         ((= i 5) vec)
			       (vector-set! vec i i))");
            this.Run("25", "do", 
            @"
            (let ((x '(1 3 5 7 9)))
     	       (do ((x x (cdr x))
     		        (sum 0 (+ sum (car x))))
     		   ((null? x) sum)))");
            this.Run("25", "do", 
            @"
            (let ((x '(1 3 5 7 9))
                  (sum 0))
     	       (do ((x x (cdr x)))
    		       ((null? x))
    		       (set! sum (+ sum (car x))))
    	       sum)");
            this.Run("1", "let", "(let foo () 1)");
            this.Run("((6 1 3) (-5 -2))", "let", 
            @"
              (let loop ((numbers '(3 -2 1 6 -5))
        		 (nonneg '())
        		 (neg '()))
        	  (cond ((null? numbers) (list nonneg neg))
        	      ((negative? (car numbers))
        	       (loop (cdr numbers)
        		     nonneg
        		     (cons (car numbers) neg)))
        	      (else
        	       (loop (cdr numbers)
        		     (cons (car numbers) nonneg)
       		     neg))))");
            this.Run("-1", "let", "(let ((f -)) (let f ((n (f 1))) n))");
        }

        /// <summary>
        /// Test quasi quote
        /// </summary>
        [TestMethod]
        public void QuasiQuoteTest()
        {
            this.section = "4.2.6";
            this.LoadTest();
            this.Run("(list 3 4)", "quasiquote", "`(list ,(+ 1 2) 4)");
            this.Run("(list a 'a)", "quasiquote", "(let ((name 'a)) `(list ,name ',name))");
            this.Run("(a 3 4 5 6 b)", "quasiquote", "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)");
            this.Run("((foo 7) . cons)", "quasiquote", 
	                               "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))");
            this.ReadAndEvaluate(@"(define (sqt x)
	                          (do ((i 0 (+ i 1)))
	                             ((> (* i i) x) (- i 1))))");

            this.Run("#(10 5 2 4 3 8)", "quasiquote", "`#(10 5 ,(sqt 4) ,@(map sqt '(16 9)) 8)");
            this.Run("5", "quasiquote", "`,(+ 2 3)");
            this.Run("(a `(b ,(+ 1 2) ,(foo 4 d) e) f)", "quasiquote", "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)");

            this.RunTest(@"(test '(a `(b ,x ,'y d) e) 'quasiquote
	                           (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)))
                                    (test '(list 3 4) 'quasiquote (quasiquote (list (unquote (+ 1 2)) 4)))");

            this.Run("`(list ,(+ 1 2) 4)", "quasiquote", "'(quasiquote (list (unquote (+ 1 2)) 4))");
        }

        /// <summary>
        /// Test begin some more
        /// </summary>
        [TestMethod]
        public void BeginMoreTest()
        {
            this.section = "5.2.1";
            this.LoadTest();
            this.Run("#t", "tprint", 
              @"(define (tprint x) #t)
                    (tprint 56)");
            this.Run("6", "define", 
                  @"(define add3 (lambda (x) (+ x 3)))
                        (add3 3)");
            this.Run("1", "define", 
                  @"(define first car)
                    (first '(1 2))");
            this.Run("9", "define", 
                  @"(define foo (lambda () 9))
                    (foo)");
            this.Run("9", "define", 
                  @"(define foo foo)
                    (foo)");
            this.Run("10", "define", 
                @"(define foo (let ((foo foo)) (lambda () (+ 1 (foo)))))
                  (foo)");

            this.Run("9", "add3", 
                         @"(define old-+ +)
                           (begin (begin (begin)
	                            (begin (begin (begin) (define + (lambda (x y) (list y x)))
			                        (begin)))
	                            (begin))
                           (begin)
                          (begin (begin (begin) (test '(3 6) add3 6)
		                      (begin))))
                         (set! + old-+)
                         (add3 6)");
            this.ReadAndEvaluate(
                         @"(begin)
                           (begin (begin))
                           (begin (begin (begin (begin))))");
        }

        /// <summary>
        /// Test define some more
        /// </summary>
        [TestMethod]
        public void DefineMoreTest()
        {
            this.section = "5.2.2";
            this.Run("45", "define", 
                            @"
                              (let ((x 5))
	                              (begin (begin (begin)
		                             (begin (begin (begin) (define foo (lambda (y) (bar x y)))
				                        (begin)))
		                             (begin))
	                             (begin)
	                             (begin)
	                         (begin (define bar (lambda (a b) (+ (* a b) a))))
	                             (begin))
	                   (begin)
	                  (begin (foo (+ x 3))))");
            this.Run("5", "foo", 
                         @"(define x 34)
                           (define (foo) (define x 5) x)
                           (foo)");
            this.Run("34", "define", "x");
            this.Run("5", "foo", 
                         @"(define foo (lambda () (define x 5) x))
                           (foo)");
            this.Run("34", "define", "x");
            this.Run("88", "foo", 
                         @"(define (foo x) ((lambda () (define x 5) x)) x)
                           (foo 88)");
            this.Run("4", "foo", "(foo 4)");
            this.Run("34", "define", "x");
            this.Run("99", "internal-define", 
                @"(letrec ((foo (lambda (arg)
					  (or arg (and (procedure? foo)
						       (foo 99))))))
			    (define bar (foo #f))
			    (foo #f))");
            this.Run("77", "internal-define", 
              @"(letrec ((foo 77)
				   (bar #f)
				   (retfoo (lambda () foo)))
			    (define baz (retfoo))
			    (retfoo))");
        }

        /// <summary>
        /// Test booleans
        /// </summary>
        [TestMethod]
        public void NotTest()
        {
            this.section = "6.1";
            this.Run("#f", "not", "(not #t)");
            this.Run("#f", "not", "(not 3)");
            this.Run("#f", "not", "(not (list 3))");
            this.Run("#t", "not", "(not #f)");
            this.Run("#f", "not", "(not '())");
            this.Run("#f", "not", "(not (list))");
            this.Run("#f", "not", "(not 'nil)");
            this.Run("#t", "boolean?", "(boolean? #f)");
            this.Run("#f", "boolean?", "(boolean? 0)");
            this.Run("#f", "boolean?", "(boolean? '())");
        }

        /// <summary>
        /// Test equivalence predicates
        /// </summary>
        [TestMethod]
        public void EquivalenceTest()
        {
            this.section = "6.2";
            this.LoadTest();

            this.Run("#t", "eqv?", "(eqv? 'a 'a)");
            this.Run("#f", "eqv?", "(eqv? 'a 'b)");
            this.Run("#t", "eqv?", "(eqv? 2 2)");
            this.Run("#t", "eqv?", "(eqv? '() '())");
            this.Run("#t", "eqv?", "(eqv? '10000 '10000)");
            this.Run("#f", "eqv?", "(eqv? (cons 1 2)(cons 1 2))");
            this.Run("#f", "eqv?", "(eqv? (lambda () 1) (lambda () 2))");
            this.Run("#f", "eqv?", "(eqv? #f 'nil)");

            this.RunTest(@"(let ((p (lambda (x) x)))
                                      (test #t eqv? p p))");
            this.RunTest(@"(define gen-counter
                           (lambda ()
                              (let ((n 0))
                                 (lambda () (set! n (+ n 1)) n))))
                                      (let ((g (gen-counter))) (test #t eqv? g g))");
            this.Run("#f", "eqv?", "(eqv? (gen-counter) (gen-counter))");
            this.RunTest(@"(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	                                (g (lambda () (if (eqv? f g) 'g 'both))))
                            (test #f eqv? f g))");

            this.Run("#t", "eq?", "(eq? 'a 'a)");
            this.Run("#f", "eq?", "(eq? (list 'a) (list 'a))");
            this.Run("#t", "eq?", "(eq? '() '())");
            this.Run("#t", "eq?", "(eq? car car)");
            this.RunTest("(let ((x '(a))) (test #t eq? x x))");
            this.RunTest("(let ((x '#())) (test #t eq? x x))");
            this.RunTest("(let ((x (lambda (x) x))) (test #t eq? x x))");

            this.ReadAndEvaluate(@"(define test-eq?-eqv?-agreement
                                      (lambda (obj1 obj2)
                                        (cond ((eq? (eq? obj1 obj2) (eqv? obj1 obj2)))
	                                     (else #f))))");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement '#f '#f)");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement '#t '#t)");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement '#t '#f)");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement '(a) '(a))");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement '(a) '(b))");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement car car)");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement car cdr)");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement (list 'a) (list 'a))");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement (list 'a) (list 'b))");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement '#(a) '#(a))");
            this.Run("#t", "test-eq?-eqv?-agreement", "(test-eq?-eqv?-agreement '#(a) '#(b))");
            this.Run("#t", "test-eq?-eqv?-agreement", @"(test-eq?-eqv?-agreement ""abc"" ""abc"")");
            this.Run("#t", "test-eq?-eqv?-agreement", @"(test-eq?-eqv?-agreement ""abc"" ""abz"")");

            this.Run("#t", "equal?", "(equal? 'a 'a)");
            this.Run("#t", "equal?", "(equal? '(a) '(a))");
            this.Run("#t", "equal?", "(equal? '(a (b) c) '(a (b) c))");
            this.Run("#t", "equal?", @"(equal? ""abc"" ""abc"")");
            this.Run("#t", "equal?", "(equal? 2 2)");
            this.Run("#t", "equal?", "(equal? (make-vector 5 'a) (make-vector 5 'a))");
        }

        /// <summary>
        /// Test pairs and lists
        /// </summary>
        [TestMethod]
        public void PairListTest()
        {
            this.section = "6.3";
            this.LoadTest();

            this.Run("(a b c d e)", "dot", @"'(a . (b . (c . (d . (e . ())))))");
            this.RunTest(@"(define x (list 'a 'b 'c))
                           (define y x)
                              (and list? (test #t list? y))");
            this.Run("(a . 4)", "set-cdr!", 
                         @"(set-cdr! x 4)
                           x");
            this.Run("#t", "eqv?", "(eqv? x y)");
            this.Run("(a b c . d)", "dot", "'(a . (b . (c . d)))");
            this.RunTest("(and list? (test #f list? y))");
            this.RunTest("(and list? (let ((x (list 'a))) (set-cdr! x x) (test #f 'list? (list? x))))");
            this.Run("#t", "pair?", "(pair? '(a . b))");
            this.Run("#t", "pair?", "(pair? '(a . 1))");
            this.Run("#t", "pair?", "(pair? '(a b c))");
            this.Run("#f", "pair?", "(pair? '())");
            this.Run("#f", "pair?", "(pair? '#(a b))");
            this.Run("(a)", "cons", "(cons 'a '())");
            this.Run("((a) b c d)", "cons", "(cons '(a) '(b c d))");
            this.Run(@"(""a"" b c)", "cons", @"(cons ""a"" '(b c))");
            this.Run("(a . 3)", "cons", "(cons 'a 3)");
            this.Run("((a b) . c)", "cons", "(cons '(a b) 'c)");

            this.Run("a", "car", "(car '(a b c))");
            this.Run("(a)", "car", "(car '((a) b c d))");
            this.Run("1", "car", "(car '(1 . 2))");

            this.Run("(b c d)", "cdr", "(cdr '((a) b c d))");
            this.Run("2", "cdr", "(cdr '(1 . 2))");

            this.Run("(a 7 c)", "list", "(list 'a (+ 3 4) 'c)");
            this.Run("()", "list", "(list)");

            this.Run("3", "length", "(length '(a b c))");
            this.Run("3", "length", "(length '(a (b) (c d e)))");
            this.Run("0", "length", "(length '())");

            this.Run("(x y)", "append", "(append '(x) '(y))");
            this.Run("(a b c d)", "append", "(append '(a) '(b c d))");
            this.Run("(a (b) (c))", "append", "(append '(a (b)) '((c)))");
            this.Run("()", "append", "(append)");
            this.Run("(a b c . d)", "append", "(append '(a b) '(c . d))");
            this.Run("a", "append", "(append '() 'a)");

            this.Run("(c b a)", "reverse", "(reverse '(a b c))");
            this.Run("((e (f)) d (b c) a)", "reverse", "(reverse '(a (b c) d (e (f))))");

            this.Run("c", "list-ref", "(list-ref '(a b c d) 2)");

            this.Run("(a 1)", "assq", @"(define e '((a 1) (b 2) (c 3)))
                                        (assq 'a e)");
            this.Run("(b 2)", "assq", "(assq 'b e)");
            this.Run("#f", "assq", "(assq 'd e)");
            this.Run("#f", "assq", "(assq (list 'a) '(((a)) ((b)) ((c))))");
            this.Run("((a))", "assoc", "(assoc (list 'a) '(((a)) ((b)) ((c))))");
            this.Run("(5 7)", "assv", "(assv 5 '((2 3) (5 7) (11 13)))");
        }

        /// <summary>
        /// Test symbols
        /// </summary>
        [TestMethod]
        public void SymbolTest()
        {
            this.section = "6.4";
            this.LoadTest();
            this.Run("#t", "symbol?", "(symbol? 'foo)");
            this.Run("#t", "symbol?", "(symbol? (car '(a b)))");
            this.Run("#f", "symbol?", @"(symbol? ""bar"")");
            this.Run("#t", "symbol?", "(symbol? 'nil)");
            this.Run("#f", "symbol?", "(symbol? '())");
            this.Run("#f", "symbol?", "(symbol? #f)");
            this.ReadAndEvaluate(@"(define char-standard-case char-upcase)
                                   (if (string=? (symbol->string 'A) ""a"")
                                           (set! char-standard-case char-downcase))");
            this.Run("#t", "standard-case", "(string=? (symbol->string 'a) (symbol->string 'A))");
            this.Run("#t", "standard-case", @"
                             (or (string=? (symbol->string 'a) ""A"")
	                             (string=? (symbol->string 'A) ""a""))");

            this.Run(@"""flying-fish""", "symbol->string", "(symbol->string 'flying-fish)");
            this.Run(@"""martin""", "symbol->string", "(symbol->string 'Martin)");
            this.Run(@"""Malvina""", "string->symbol", @"(symbol->string (string->symbol ""Malvina""))");
            this.Run("#t", "standard-case", "(eq? 'a 'A)");

            this.ReadAndEvaluate(@"(define x (string #\a #\b))
                                   (define y (string->symbol x))
                                   (string-set! x 0 #\c)");
            this.Run(@"""cb""", "string-set!", "x");
            this.Run("ab", "symbol->string", "y");
            this.RunTest(@"(test y string->symbol ""ab"")");
            this.Run("#t", "eq", @"(eq? 'mISSISSIppi 'mississippi)");
            this.Run("#f", "string->symbol", @"(eq? 'bitBlt (string->symbol ""bitBlt""))");
            this.RunTest(@"(test 'JollyWog string->symbol (symbol->string 'JollyWog))");
        }

        /// <summary>
        /// Test numbers
        /// </summary>
        [TestMethod]
        public void NumberTest()
        {
            this.section = "6.5.5";
            this.Run("#t", "number?", "(number? 3)");

            // no complex or rational
            this.Run("#t", "real?", "(real? 3)");
            this.Run("#t", "integer?", "(integer? 3)");

            this.Run("#t", "exact?", "(exact? 3)");
            this.Run("#f", "inexact?", "(inexact? 3)");

            this.Run("1", "expt", "(expt 0 0)");
            this.Run("0", "expt", "(expt 0 1)");
            this.Run("0", "expt", "(expt 0 256)");
            this.Run("0", "expt", "(expt 0 -255)");
            this.Run("1", "expt", "(expt -1 256)");
            this.Run("-1", "expt", "(expt -1 255)");
            this.Run("1", "expt", "(expt -1 -256)");
            this.Run("-1", "expt", "(expt -1 -255)");
            this.Run("1", "expt", "(expt 256 0)");
            this.Run("1", "expt", "(expt -256 0)");
            this.Run("256", "expt", "(expt 256 1)");
            this.Run("-256", "expt", "(expt -256 1)");
            this.Run("8", "expt", "(expt 2 3)");
            this.Run("-8", "expt", "(expt -2 3)");
            this.Run("9", "expt", "(expt 3 2)");
            this.Run("9", "expt", "(expt -3 2)");

            this.Run("#t", "=", "(= 22 22 22)");
            this.Run("#t", "=", "(= 22 22)");
            this.Run("#f", "=", "(= 34 34 35)");
            this.Run("#f", "=", "(= 34 35)");
            this.Run("#t", ">", "(> 3 -6246)");
            this.Run("#f", ">", "(> 9 9 -2424)");
            this.Run("#t", ">=", "(>= 3 -4 -6246)");
            this.Run("#t", ">=", "(>= 9 9)");
            this.Run("#f", ">=", "(>= 8 9)");
            this.Run("#t", "<", "(< -1 2 3 4 5 6 7 8)");
            this.Run("#f", "<", "(< -1 2 3 4 4 5 6 7)");
            this.Run("#t", "<=", "(<= -1 2 3 4 5 6 7 8)");
            this.Run("#t", "<=", "(<= -1 2 3 4 4 5 6 7)");
            this.Run("#f", "<", "(< 1 3 2)");
            this.Run("#f", ">=", "(>= 1 3 2)");

            this.Run("#t", "zero?", "(zero? 0)");
            this.Run("#f", "zero?", "(zero? 1)");
            this.Run("#f", "zero?", "(zero? -1)");
            this.Run("#f", "zero?", "(zero? -100)");
            this.Run("#t", "positive?", "(positive? 4)");
            this.Run("#f", "positive?", "(positive? -4)");
            this.Run("#f", "positive?", "(positive? 0)");
            this.Run("#f", "negative?", "(negative? 4)");
            this.Run("#t", "negative?", "(negative? -4)");
            this.Run("#f", "negative?", "(negative? 0)");
            this.Run("#t", "odd?", "(odd? 3)");
            this.Run("#f", "odd?", "(odd? 2)");
            this.Run("#f", "odd?", "(odd? -4)");
            this.Run("#t", "odd?", "(odd? -1)");
            this.Run("#f", "even?", "(even? 3)");
            this.Run("#t", "even?", "(even? 2)");
            this.Run("#t", "even?", "(even? -4)");
            this.Run("#f", "even?", "(even? -1)");

            this.Run("38", "max", "(max 34 5 7 38 6)");
            this.Run("-24", "min", "(min 3  5 5 330 4 -24)");

            this.Run("7", "+", "(+ 3 4)");
            this.Run("3", "+", "(+ 3)");
            this.Run("0", "+", "(+)");
            this.Run("4", "*", "(* 4)");
            this.Run("1", "*", "(*)");
            this.Run("1", "/", "(/ 1)");
            this.Run("-1", "/", "(/ -1)");
            this.Run("2", "/", "(/ 6 3)");
            this.Run("-3", "/", "(/ 6 -2)");
            this.Run("-3", "/", "(/ -6 2)");
            this.Run("3", "/", "(/ -6 -2)");
            this.Run("-1", "-", "(- 3 4)");
            this.Run("-3", "-", "(- 3)");
            this.Run("7", "abs", "(abs -7)");
            this.Run("7", "abs", "(abs 7)");
            this.Run("0", "abs", "(abs 0)");

            this.Run("5", "quotient", "(quotient 35 7)");
            this.Run("-5", "quotient", "(quotient -35 7)");
            this.Run("-5", "quotient", "(quotient 35 -7)");
            this.Run("5", "quotient", "(quotient -35 -7)");
            this.Run("1", "modulo", "(modulo 13 4)");
            this.Run("1", "remainder", "(remainder 13 4)");
            this.Run("3", "modulo", "(modulo -13 4)");
            this.Run("-1", "remainder", "(remainder -13 4)");
            this.Run("-3", "modulo", "(modulo 13 -4)");
            this.Run("1", "remainder", "(remainder 13 -4)");
            this.Run("-1", "modulo", "(modulo -13 -4)");
            this.Run("-1", "remainder", "(remainder -13 -4)");
            this.Run("0", "modulo", "(modulo 0 86400)");
            this.Run("0", "modulo", "(modulo 0 -86400)");
            this.ReadAndEvaluate(@"(define (divtest n1 n2)
	                                (= n1 (+ (* n2 (quotient n1 n2))
		                                 (remainder n1 n2))))");
            this.Run("#t", "divtest", "(divtest 238 9)");
            this.Run("#t", "divtest", "(divtest -238 9)");
            this.Run("#t", "divtest", "(divtest 238 -9)");
            this.Run("#t", "divtest", "(divtest -238 -9)");

            this.Run("4", "gcd", "(gcd 0 4)");
            this.Run("4", "gcd", "(gcd -4 0)");
            this.Run("4", "gcd", "(gcd 32 -36)");
            this.Run("0", "gcd", "(gcd)");
            this.Run("288", "lcm", "(lcm 32 -36)");
            this.Run("1", "lcm", "(lcm)");
        }

        /// <summary>
        /// Test numbers -- more
        /// </summary>
        [TestMethod]
        public void MoreNumberTest()
        {
            this.section = "6.5.5";
            this.ReadAndEvaluate(@"(define (test-string->number str)
                                     (define ans (string->number str))
                                        (cond ((not ans) #t) ((number? ans) #t) (else ans)))");
            this.Run("#t", "test-string->number", @"(test-string->number ""+#.#"")");
            this.Run("#t", "test-string->number", @"(test-string->number ""-#.#"")");
            this.Run("#t", "test-string->number", @"(test-string->number ""#.#"")");
            this.Run("#t", "test-string->number", @"(test-string->number ""1/0"")");
            this.Run("#t", "test-string->number", @"(test-string->number ""-1/0"")");
            this.Run("#t", "test-string->number", @"(test-string->number ""0/0"")");
            this.Run("#t", "test-string->number", @"(test-string->number ""#e"")");
            this.Run("#t", "test-string->number", @"(test-string->number ""#"")");
        }

        // No inexact number support
        // No bignum support

        /// <summary>
        /// Test string to number
        /// </summary>
        [TestMethod]
        public void StringToNumberTest()
        {
            this.section = "6.5.9";
            this.Run(@"""0""", "number->string", "(number->string 0)");
            this.Run(@"""100""", "number->string", "(number->string 100)");
            this.Run(@"""100""", "number->string", "(number->string 256 16)");
            this.Run("100", "string->number", @"(string->number ""100"")");
            this.Run("256", "string->number", @"(string->number ""100"" 16)");
            this.Run("#f", "string->number", @"(string->number """")");
            this.Run("#f", "string->number", @"(string->number ""."")");
            this.Run("#f", "string->number", @"(string->number ""d"")");
            this.Run("#f", "string->number", @"(string->number ""D"")");
            this.Run("#f", "string->number", @"(string->number ""-"")");
            this.Run("#f", "string->number", @"(string->number ""+"")");
            this.Run("#t", "string->number", @"(or (not (string->number ""80000000"" 16))
			                                         (positive? (string->number ""80000000"" 16)))");
            this.Run("#t", "string->number", @"(or (not (string->number ""-80000000"" 16))
			                                             (negative? (string->number ""-80000000"" 16)))");

            this.Run(@"""0""", "number->string", "(number->string 0)");
            this.Run(@"""100""", "number->string", "(number->string 100)");
            this.Run(@"""100""", "number->string", "(number->string 256 16)");
            this.Run("100", "number->string", @"(string->number ""100"")");
            this.Run("256", "number->string", @"(string->number ""100"" 16)");
            this.Run("#f", "string->number", @"(string->number """")");
            this.Run("#f", "string->number", @"(string->number ""."")");
            this.Run("#f", "string->number", @"(string->number ""d"")");
            this.Run("#f", "string->number", @"(string->number ""D"")");
            this.Run("#f", "string->number", @"(string->number ""-"")");
            this.Run("#f", "string->number", @"(string->number ""+"")");
            this.Run("#t", "string->number", @"(or (not (string->number ""80000000"" 16))
			                                         (positive? (string->number ""80000000"" 16)))");
            this.Run("#t", "string->number", @"(or (not (string->number ""-80000000"" 16))
			                                         (negative? (string->number ""-80000000"" 16)))");
        }

        /// <summary>
        /// Test characters
        /// </summary>
        [TestMethod]
        public void CharacterTest()
        {
            this.section = "6.6";
            this.Run("#t", "eqv?", @"(eqv? '#\  #\Space)");
            this.Run("#t", "eqv?", @"(eqv? #\space '#\Space)");
            this.Run("#t", "char?", @"(char? #\a)");
            this.Run("#t", "char?", @"(char? #\()");
            this.Run("#t", "char?", @"(char? #\space)");
            this.Run("#t", "char?", @"(char? '#\newline)");

            this.Run("#f", "char=?", @"(char=? #\A #\B)");
            this.Run("#f", "char=?", @"(char=? #\a #\b)");
            this.Run("#f", "char=?", @"(char=? #\9 #\0)");
            this.Run("#t", "char=?", @"(char=? #\A #\A)");

            this.Run("#t", "char<?", @"(char<? #\A #\B)");
            this.Run("#t", "char<?", @"(char<? #\a #\b)");
            this.Run("#f", "char<?", @"(char<? #\9 #\0)");
            this.Run("#f", "char<?", @"(char<? #\A #\A)");

            this.Run("#f", "char>?", @"(char>? #\A #\B)");
            this.Run("#f", "char>?", @"(char>? #\a #\b)");
            this.Run("#t", "char>?", @"(char>? #\9 #\0)");
            this.Run("#f", "char>?", @"(char>? #\A #\A)");

            this.Run("#t", "char<=?", @"(char<=? #\A #\B)");
            this.Run("#t", "char<=?", @"(char<=? #\a #\b)");
            this.Run("#f", "char<=?", @"(char<=? #\9 #\0)");
            this.Run("#t", "char<=?", @"(char<=? #\A #\A)");

            this.Run("#f", "char>=?", @"(char>=? #\A #\B)");
            this.Run("#f", "char>=?", @"(char>=? #\a #\b)");
            this.Run("#t", "char>=?", @"(char>=? #\9 #\0)");
            this.Run("#t", "char>=?", @"(char>=? #\A #\A)");

            this.Run("#f", "char-ci=?", @"(char-ci=? #\A #\B)");
            this.Run("#f", "char-ci=?", @"(char-ci=? #\a #\B)");
            this.Run("#f", "char-ci=?", @"(char-ci=? #\A #\b)");
            this.Run("#f", "char-ci=?", @"(char-ci=? #\a #\b)");
            this.Run("#f", "char-ci=?", @"(char-ci=? #\9 #\0)");
            this.Run("#t", "char-ci=?", @"(char-ci=? #\A #\A)");
            this.Run("#t", "char-ci=?", @"(char-ci=? #\A #\a)");

            this.Run("#t", "char-ci<?", @"(char-ci<? #\A #\B)");
            this.Run("#t", "char-ci<?", @"(char-ci<? #\a #\B)");
            this.Run("#t", "char-ci<?", @"(char-ci<? #\A #\b)");
            this.Run("#t", "char-ci<?", @"(char-ci<? #\a #\b)");
            this.Run("#f", "char-ci<?", @"(char-ci<? #\9 #\0)");
            this.Run("#f", "char-ci<?", @"(char-ci<? #\A #\A)");
            this.Run("#f", "char-ci<?", @"(char-ci<? #\A #\a)");

            this.Run("#f", "char-ci>?", @"(char-ci>? #\A #\B)");
            this.Run("#f", "char-ci>?", @"(char-ci>? #\a #\B)");
            this.Run("#f", "char-ci>?", @"(char-ci>? #\A #\b)");
            this.Run("#f", "char-ci>?", @"(char-ci>? #\a #\b)");
            this.Run("#t", "char-ci>?", @"(char-ci>? #\9 #\0)");
            this.Run("#f", "char-ci>?", @"(char-ci>? #\A #\A)");
            this.Run("#f", "char-ci>?", @"(char-ci>? #\A #\a)");

            this.Run("#t", "char-ci<=?", @"(char-ci<=? #\A #\B)");
            this.Run("#t", "char-ci<=?", @"(char-ci<=? #\a #\B)");
            this.Run("#t", "char-ci<=?", @"(char-ci<=? #\A #\b)");
            this.Run("#t", "char-ci<=?", @"(char-ci<=? #\a #\b)");
            this.Run("#f", "char-ci<=?", @"(char-ci<=? #\9 #\0)");
            this.Run("#t", "char-ci<=?", @"(char-ci<=? #\A #\A)");
            this.Run("#t", "char-ci<=?", @"(char-ci<=? #\A #\a)");

            this.Run("#f", "char-ci>=?", @"(char-ci>=? #\A #\B)");
            this.Run("#f", "char-ci>=?", @"(char-ci>=? #\a #\B)");
            this.Run("#f", "char-ci>=?", @"(char-ci>=? #\A #\b)");
            this.Run("#f", "char-ci>=?", @"(char-ci>=? #\a #\b)");
            this.Run("#t", "char-ci>=?", @"(char-ci>=? #\9 #\0)");
            this.Run("#t", "char-ci>=?", @"(char-ci>=? #\A #\A)");
            this.Run("#t", "char-ci>=?", @"(char-ci>=? #\A #\a)");

            this.Run("#t", "char-alphabetic?", @"(char-alphabetic? #\a)");
            this.Run("#t", "char-alphabetic?", @"(char-alphabetic? #\A)");
            this.Run("#t", "char-alphabetic?", @"(char-alphabetic? #\z)");
            this.Run("#t", "char-alphabetic?", @"(char-alphabetic? #\Z)");
            this.Run("#f", "char-alphabetic?", @"(char-alphabetic? #\0)");
            this.Run("#f", "char-alphabetic?", @"(char-alphabetic? #\9)");
            this.Run("#f", "char-alphabetic?", @"(char-alphabetic? #\space)");
            this.Run("#f", "char-alphabetic?", @"(char-alphabetic? #\;)");

            this.Run("#f", "char-numeric?", @"(char-numeric? #\a)");
            this.Run("#f", "char-numeric?", @"(char-numeric? #\A)");
            this.Run("#f", "char-numeric?", @"(char-numeric? #\z)");
            this.Run("#f", "char-numeric?", @"(char-numeric? #\Z)");
            this.Run("#t", "char-numeric?", @"(char-numeric? #\0)");
            this.Run("#t", "char-numeric?", @"(char-numeric? #\9)");
            this.Run("#f", "char-numeric?", @"(char-numeric? #\space)");
            this.Run("#f", "char-numeric?", @"(char-numeric? #\;)");

            this.Run("#f", "char-whitespace?", @"(char-whitespace? #\a)");
            this.Run("#f", "char-whitespace?", @"(char-whitespace? #\A)");
            this.Run("#f", "char-whitespace?", @"(char-whitespace? #\z)");
            this.Run("#f", "char-whitespace?", @"(char-whitespace? #\Z)");
            this.Run("#f", "char-whitespace?", @"(char-whitespace? #\0)");
            this.Run("#f", "char-whitespace?", @"(char-whitespace? #\9)");
            this.Run("#t", "char-whitespace?", @"(char-whitespace? #\space)");
            this.Run("#f", "char-whitespace?", @"(char-whitespace? #\;)");

            this.Run("#f", "char-upper-case?", @"(char-upper-case? #\0)");
            this.Run("#f", "char-upper-case?", @"(char-upper-case? #\9)");
            this.Run("#f", "char-upper-case?", @"(char-upper-case? #\space)");
            this.Run("#f", "char-upper-case?", @"(char-upper-case? #\;)");

            this.Run("#f", "char-lower-case?", @"(char-lower-case? #\0)");
            this.Run("#f", "char-lower-case?", @"(char-lower-case? #\9)");
            this.Run("#f", "char-lower-case?", @"(char-lower-case? #\space)");
            this.Run("#f", "char-lower-case?", @"(char-lower-case? #\;)");

            this.Run("#\\.", "integer->char?", @"(integer->char (char->integer #\.))");
            this.Run("#\\A", "integer->char?", @"(integer->char (char->integer #\A))");
            this.Run("#\\a", "integer->char?", @"(integer->char (char->integer #\a))");
            this.Run("#\\A", "char-upcase?", @"(char-upcase #\A)");
            this.Run("#\\A", "char-upcase?", @"(char-upcase #\a)");
            this.Run("#\\a", "char-downcase?", @"(char-downcase #\A)");
            this.Run("#\\a", "char-downcase?", @"(char-downcase #\a)");
        }

        /// <summary>
        /// Test strings
        /// </summary>
        [TestMethod]
        public void StringTest()
        {
            this.section = "6.7";
            this.LoadTest();
            this.Run("#t", "string?", @"(string? ""The word \""recursion\\\"" has many meanings."")");
            this.Run("#t", "string?", @"(string? """")");

            this.ReadAndEvaluate(@"(define f (make-string 3 #\*))
                               (test ""?**"" 'string-set! (begin (string-set! f 0 #\?) f))");
            this.Run(@"""abc""", "string", @"(string #\a #\b #\c)");

            this.Run(@"""""", "string", @"(string)");
            this.Run("3", "string-length", @"(string-length ""abc"")");
            this.Run("#\\a", "string-ref", @"(string-ref ""abc"" 0)");
            this.Run("#\\c", "string-ref", @"(string-ref ""abc"" 2)");
            this.Run("0", "string-length", @"(string-length """")");
            this.Run(@"""""", "substring", @"(substring ""ab"" 0 0)");
            this.Run(@"""""", "substring", @"(substring ""ab"" 1 1)");
            this.Run(@"""""", "substring", @"(substring ""ab"" 2 2)");
            this.Run(@"""a""", "substring", @"(substring ""ab"" 0 1)");
            this.Run(@"""b""", "substring", @"(substring ""ab"" 1 2)");
            this.Run(@"""ab""", "substring", @"(substring ""ab"" 0 2)");

            this.Run(@"""foobar""", "string-append", @"(string-append ""foo"" ""bar"")");
            this.Run(@"""foo""", "string-append", @"(string-append ""foo"")");
            this.Run(@"""foo""", "string-append", @"(string-append ""foo"" """")");
            this.Run(@"""foo""", "string-append", @"(string-append """" ""foo"")");
            this.Run(@"""""", "string-append", @"(string-append)");
            this.Run(@"""""", "make-string", @"(make-string 0)");
            this.Run("#t", "string=?", @"(string=? """" """")");
            this.Run("#f", "string<?", @"(string<? """" """")");
            this.Run("#f", "string>?", @"(string>? """" """")");
            this.Run("#t", "string<=?", @"(string<=? """" """")");
            this.Run("#t", "string>=?", @"(string>=? """" """")");
            this.Run("#t", "string-ci=?", @"(string-ci=? """" """")");
            this.Run("#f", "string-ci<=?", @"(string-ci<? """" """")");
            this.Run("#f", "string-ci>=?", @"(string-ci>? """" """")");
            this.Run("#t", "string-ci<=?", @"(string-ci<=? """" """")");
            this.Run("#t", "string-ci>=?", @"(string-ci>=? """" """")");

            this.Run("#f", "string=?", @"(string=? ""A"" ""B"")");
            this.Run("#f", "string=?", @"(string=? ""a"" ""b"")");
            this.Run("#f", "string=?", @"(string=? ""9"" ""0"")");
            this.Run("#t", "string=?", @"(string=? ""A"" ""A"")");
            this.Run("#t", "string<?", @"(string<? ""A"" ""B"")");
            this.Run("#t", "string<?", @"(string<? ""a"" ""b"")");
            this.Run("#f", "string<?", @"(string<? ""9"" ""0"")");
            this.Run("#f", "string<?", @"(string<? ""A"" ""A"")");

            this.Run("#t", "string<?", @"(string<? ""A"" ""AB"")");
            this.Run("#f", "string<?", @"(string<? ""AB"" ""A"")");
            this.Run("#f", "string<?", @"(string<? ""Bxx"" ""Axx"")");
            this.Run("#f", "string<?", @"(string<? ""Bxxx"" ""Axxx"")");
            this.Run("#t", "string<?", @"(string<? ""Axx"" ""Bxx"")");
            this.Run("#t", "string<?", @"(string<? ""Axxx"" ""Bxxx"")");

            this.Run("#f", "string>?", @"(string>? ""A"" ""B"")");
            this.Run("#f", "string>?", @"(string>? ""a"" ""b"")");
            this.Run("#t", "string>?", @"(string>? ""9"" ""0"")");
            this.Run("#f", "string>?", @"(string>? ""A"" ""A"")");

            this.Run("#t", "string<=?", @"(string<=? ""A"" ""B"")");
            this.Run("#t", "string<=?", @"(string<=? ""a"" ""b"")");
            this.Run("#f", "string<=?", @"(string<=? ""9"" ""0"")");
            this.Run("#t", "string<=?", @"(string<=? ""A"" ""A"")");

            this.Run("#f", "string>=?", @"(string>=? ""A"" ""B"")");
            this.Run("#f", "string>=?", @"(string>=? ""a"" ""b"")");
            this.Run("#t", "string>=?", @"(string>=? ""9"" ""0"")");
            this.Run("#t", "string>=?", @"(string>=? ""A"" ""A"")");

            this.Run("#f", "string-ci=?", @"(string-ci=? ""A"" ""B"")");
            this.Run("#f", "string-ci=?", @"(string-ci=? ""a"" ""B"")");
            this.Run("#f", "string-ci=?", @"(string-ci=? ""A"" ""b"")");
            this.Run("#f", "string-ci=?", @"(string-ci=? ""a"" ""b"")");
            this.Run("#f", "string-ci=?", @"(string-ci=? ""9"" ""0"")");
            this.Run("#t", "string-ci=?", @"(string-ci=? ""A"" ""A"")");
            this.Run("#t", "string-ci=?", @"(string-ci=? ""A"" ""a"")");

            this.Run("#t", "string-ci<?", @"(string-ci<? ""A"" ""B"")");
            this.Run("#t", "string-ci<?", @"(string-ci<? ""a"" ""B"")");
            this.Run("#t", "string-ci<?", @"(string-ci<? ""A"" ""b"")");
            this.Run("#t", "string-ci<?", @"(string-ci<? ""a"" ""b"")");
            this.Run("#f", "string-ci<?", @"(string-ci<? ""9"" ""0"")");
            this.Run("#f", "string-ci<?", @"(string-ci<? ""A"" ""A"")");
            this.Run("#f", "string-ci<?", @"(string-ci<? ""A"" ""a"")");

            this.Run("#f", "string-ci>?", @"(string-ci>? ""A"" ""B"")");
            this.Run("#f", "string-ci>?", @"(string-ci>? ""a"" ""B"")");
            this.Run("#f", "string-ci>?", @"(string-ci>? ""A"" ""b"")");
            this.Run("#f", "string-ci>?", @"(string-ci>? ""a"" ""b"")");
            this.Run("#t", "string-ci>?", @"(string-ci>? ""9"" ""0"")");
            this.Run("#f", "string-ci>?", @"(string-ci>? ""A"" ""A"")");
            this.Run("#f", "string-ci>?", @"(string-ci>? ""A"" ""a"")");

            this.Run("#t", "string-ci<=?", @"(string-ci<=? ""A"" ""B"")");
            this.Run("#t", "string-ci<=?", @"(string-ci<=? ""a"" ""B"")");
            this.Run("#t", "string-ci<=?", @"(string-ci<=? ""A"" ""b"")");
            this.Run("#t", "string-ci<=?", @"(string-ci<=? ""a"" ""b"")");
            this.Run("#f", "string-ci<=?", @"(string-ci<=? ""9"" ""0"")");
            this.Run("#t", "string-ci<=?", @"(string-ci<=? ""A"" ""A"")");
            this.Run("#t", "string-ci<=?", @"(string-ci<=? ""A"" ""a"")");

            this.Run("#f", "string-ci>=?", @"(string-ci>=? ""A"" ""B"")");
            this.Run("#f", "string-ci>=?", @"(string-ci>=? ""a"" ""B"")");
            this.Run("#f", "string-ci>=?", @"(string-ci>=? ""A"" ""b"")");
            this.Run("#f", "string-ci>=?", @"(string-ci>=? ""a"" ""b"")");
            this.Run("#t", "string-ci>=?", @"(string-ci>=? ""9"" ""0"")");
            this.Run("#t", "string-ci>=?", @"(string-ci>=? ""A"" ""A"")");
            this.Run("#t", "string-ci>=?", @"(string-ci>=? ""A"" ""a"")");
        }

        /// <summary>
        /// Test vectors
        /// </summary>
        [TestMethod]
        public void VectorTest()
        {
            this.section = "6.8";
            this.Run("#t", "vector?", @"(vector? '#(0 (2 2 2 2) ""Anna:""))");
            this.Run("#t", "vector?", @"(vector? '#())");
            this.Run("#(a b c)", "vector", @"(vector 'a 'b 'c)");
            this.Run("#()", "vector", "(vector)");
            this.Run("3", "vector-length", @"(vector-length '#(0 (2 2 2 2) ""Anna""))");
            this.Run("0", "vector-length", @"(vector-length '#())");
            this.Run("8", "vector-ref", @"(vector-ref '#(1 1 2 3 5 8 13 21) 5)");
            this.Run(@"#(0 (""Sue"" ""Sue"") ""Anna"")", "vector-set", 
                         @"(let ((vec (vector 0 '(2 2 2 2) ""Anna"")))
	                                 (vector-set! vec 1 '(""Sue"" ""Sue""))
	                               vec)");
            this.Run("#(hi hi)", "make-vector", @"(make-vector 2 'hi)");
            this.Run("#()", "make-vector", @"(make-vector 0)");
            this.Run("#()", "make-vector", @"(make-vector 0 'a) ");
        }

        /// <summary>
        /// Test control features
        /// </summary>
        [TestMethod]
        public void ControlFeaturesTest()
        {
            this.section = "6.9";
            this.Run("#t", "procedure?", "(procedure? car)");
            this.Run("#f", "procedure?", "(procedure? 'car)");
            this.Run("#t", "procedure?", "(procedure? (lambda (x) (* x x)))");
            this.Run("#f", "procedure?", "(procedure? '(lambda (x) (* x x)))");
            this.Run("#t", "call-with-current-continuation", "(call-with-current-continuation procedure?)");
            this.Run("#t", "procedure?", "(procedure? /)");
            this.Run("7", "apply", "(apply + (list 3 4))");
            this.Run("7", "apply", "(apply (lambda (a b) (+ a b)) (list 3 4))");
            this.Run("17", "apply", "(apply + 10 (list 3 4))");
            this.Run("()", "apply", "(apply list '())");
            this.ReadAndEvaluate("(define compose (lambda (f g) (lambda args (f (apply g args)))))");
            this.Run("30", "compose", "((compose sqrt *) 12 75)");

            this.Run("(b e h)", "map", @"(map cadr '((a b) (d e) (g h)))");
            this.Run("(5 7 9)", "map", @"( map + '(1 2 3) '(4 5 6))");
            this.Run("(1 2 3)", "map", @"(map + '(1 2 3))");
            this.Run("(1 2 3)", "map", @"(map * '(1 2 3))");
            this.Run("(-1 -2 -3)", "map", @"(map - '(1 2 3))");

            this.Run("#(0 1 4 9 16)", "for-each", 
                            @"
                            (let ((v (make-vector 5)))
	                          (for-each (lambda (i) (vector-set! v i (* i i)))
		                          '(0 1 2 3 4))
	                          v)");
            this.Run("-3", "call-with-current-continuation", 
                          @"(call-with-current-continuation
                             (lambda (exit)
	                           (for-each (lambda (x) (if (negative? x) (exit x)))
		                            '(54 0 37 -3 245 19))
	                           #t))");
            this.ReadAndEvaluate(@"(define list-length
                                  (lambda (obj)
                                    (call-with-current-continuation
                                       (lambda (return)
                                          (letrec ((r (lambda (obj) (cond ((null? obj) 0)
				                                      ((pair? obj) (+ (r (cdr obj)) 1))
				                                      (else (return #f))))))
	                                (r obj))))))");
            this.Run("4", "list-length", @"(list-length '(1 2 3 4))");
            this.Run("#f", "list-length", @"(list-length '(a b . c))");
            this.Run("()", "map", @"(map cadr '())");
        }

        /// <summary>
        /// Test call/cc
        /// </summary>
        [TestMethod]
        public void CallCcTest()
        {
            this.section = "6.9";
            this.ReadAndEvaluate(@"
            (define (next-leaf-generator obj eot)
              (letrec ((return #f)
	                   (cont (lambda (x)
		                         (recur obj)
		                         (set! cont (lambda (x) (return eot)))
		                       (cont #f)))
	                   (recur (lambda (obj)
		                  (if (pair? obj)
			              (for-each recur obj)
			              (call-with-current-continuation
			               (lambda (c)
			                 (set! cont c)
			                 (return obj)))))))
                (lambda () (call-with-current-continuation
		            (lambda (ret) (set! return ret) (cont #f))))))
            (define (leaf-eq? x y)
              (let* ((eot (list 'eot))
	                 (xf (next-leaf-generator x eot))
	                 (yf (next-leaf-generator y eot)))
                (letrec ((loop (lambda (x y)
		                 (cond ((not (eq? x y)) #f)
			                   ((eq? eot x) #t)
			                   (else (loop (xf) (yf)))))))
                  (loop (xf) (yf)))))
            ");
            this.Run("#t", "leff-eq", "(leaf-eq? '(a (b (c))) '((a) b c))");
            this.Run("#f", "leaf-eq", "(leaf-eq? '(a (b (c))) '((a) b c d))");
        }

        /// <summary>
        /// Test force and delay
        /// </summary>
        [TestMethod]
        public void ForceDelayTest()
        {
            this.section = "6.9";
            this.LoadTest(); 
            this.Run("3", "delay", @"(force (delay (+ 1 2)))");
            this.Run("(3 3)", "delay", 
            @"(let ((p (delay (+ 1 2))))
			   (list (force p) (force p)))");

            this.Run("2", "delay", 
              @"(letrec ((a-stream
			       (letrec ((next (lambda (n)
					        (cons n (delay (next (+ n 1)))))))
			         (next 0)))
			      (head car)
			      (tail (lambda (stream) (force (cdr stream)))))
		        (head (tail (tail a-stream))))");

            this.RunTest(@"
                (letrec ((count 0)
	               (p (delay (begin (set! count (+ count 1))
			                (if (> count x)
				            count
				            (force p)))))
	               (x 5))
                (test 6 force p)
                (set! x 10)
                (test 6 force p))");
            this.Run("3", "force", @"
	            (letrec ((p (delay (if c 3 (begin (set! c #t) (+ (force p) 1)))))
		             (c #f))
	              (force p))");
        }

        /// <summary>
        /// Test input port
        /// </summary>
        [TestMethod]
        public void InputPortTest()
        {
            this.section = "6.10.1";
            new StreamWriter("temp.scm").Close();   // create a file
            this.Run("#t", "input-port?", @"(input-port? (current-input-port))");
            this.Run("#t", "output-port?", @"(output-port? (current-output-port))");
            this.Run("#t", "call-with-input-file", @"(call-with-input-file ""temp.scm"" input-port?)");
            this.Run("#t", "input-port?", 
                 @"(define this-file (open-input-file ""temp.scm""))
                       (input-port? this-file)");
            this.ReadAndEvaluate("(close-input-port this-file)");
        }

        /// <summary>
        /// Test input read
        /// </summary>
        [TestMethod]
        public void InputReadTest()
        {
            TextWriter writer = new StreamWriter("temp.scm");
            writer.WriteLine(";;");
            writer.WriteLine("(define cur-section '())(define errs '())");
            writer.Close();
            this.section = "6.10.2";
            this.ReadAndEvaluate(@"(define this-file (open-input-file ""temp.scm""))");
            this.Run("#\\;", "peek-char", @"(peek-char this-file)");
            this.Run("#\\;", "peek-char", @"(peek-char this-file)");
            this.Run("#\\;", "read-char", @"(read-char this-file)");
            this.Run("(define cur-section '())", "read", @"(read this-file)");
            this.Run("#\\(", "peek-char", @"(peek-char this-file)");
            this.Run("(define errs '())", "read", @"(read this-file)");
            this.ReadAndEvaluate(@"(close-input-port this-file)");
            this.ReadAndEvaluate(@"(close-input-port this-file)");
        }

        /// <summary>
        /// Test output write
        /// </summary>
        [TestMethod]
        public void OutputWriteTest()
        {
            TextWriter writer = new StreamWriter("temp.scm");
            writer.WriteLine(";;");
            writer.WriteLine("(define cur-section '())(define errs '())");
            writer.Close();
            this.section = "6.10.3";
            this.LoadTest();
            this.ReadAndEvaluate(@"
             (define (check-test-file name)
                  (define test-file (open-input-file name))
                     (test #t 'input-port?
                      	(call-with-input-file
              	    name
           	    (lambda (test-file)
          	      (test load-test-obj read test-file)
         	      (test #t eof-object? (peek-char test-file))
        	      (test #t eof-object? (read-char test-file))
        	      (input-port? test-file))))
                (test #\; read-char test-file)
                (test #\; read-char test-file)
                (test #\; read-char test-file)
                (test write-test-obj read test-file)
                (test load-test-obj read test-file)
                (close-input-port test-file)
                #t)
              (define write-test-obj
                 '(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c)))
             (define load-test-obj
                 (list 'define 'foo (list 'quote write-test-obj)))");
            this.Run("#t", "call-with-output-file", 
                    @"(call-with-output-file
                         ""tmp1""
                         (lambda (test-file)
	                        (write-char #\; test-file)
	                        (display #\; test-file)
	                        (display "";"" test-file)
	                        (write write-test-obj test-file)
	                        (newline test-file)
	                        (write load-test-obj test-file)
	                        (output-port? test-file)))");
            this.Run("#t", "check-test-file", @"(check-test-file ""tmp1"")");

            this.ReadAndEvaluate(@"(define test-file (open-output-file ""tmp2""))
                              (write-char #\; test-file)
                              (display #\; test-file)
                              (display "";"" test-file)
                              (write write-test-obj test-file)
                              (newline test-file)
                              (write load-test-obj test-file)");
            this.Run("#t", "output-port?", "(output-port? test-file)");
            this.Run("#t", "close-output-port", @"(close-output-port test-file)
                               (check-test-file ""tmp2"")");
            Assert.AreEqual(EmptyList.Instance, this.ReadAndEvaluate("errs"));
        }

        /// <summary>
        /// Test new to r4
        /// </summary>
        [TestMethod]
        public void R4Test()
        {
            this.section = "6.7";
            this.LoadTest();
            this.Run(@"(#\P #\space #\l)", "string->list", @"(string->list ""P l"")");
            this.Run("()", "string->list", @"(string->list """")");
            this.Run(@"""1\\""""", "list->string", @"(list->string '(#\1 #\\ #\""))");
            this.Run(@"""""", "list->string", @"(list->string '())");

            this.section = "6.8";
            this.Run("(dah dah didah)", "vector->list", @"(vector->list '#(dah dah didah))");
            this.Run("()", "vector->list", @"(vector->list '#())");
            this.Run("#(dididit dah)", "list->vector", @"(list->vector '(dididit dah))");
            this.Run("#()", "list->vector", @"(list->vector '())");

            this.section = "6.10.4";
            TextWriter writer = new StreamWriter("tmp1");
            writer.WriteLine(@";;;(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c))
                     (define foo '(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c)))");
            writer.Close();
            this.Run("<undefined>", "loca", @"(load ""tmp1"")");
            this.ReadAndEvaluate(@"(define write-test-obj
                 '(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c)))");
            this.Run("#t", "loca", @"(test write-test-obj 'load foo)");
        }

        /// <summary>
        /// Load the test function in.
        /// </summary>
        private void LoadTest()
        {
            this.ReadAndEvaluate(@"
              (define errs '())
              (define record-error (lambda (e) (set! errs (cons e errs))))
              (define test
                (lambda (expect fun . args)
                  (write (cons fun args))
                  (display ""  ==> "")
                  ((lambda (res)
                      (write res)
                      (newline)
                      (cond ((not (equal? expect res))
	                     (record-error (list res expect (cons fun args)))
	                     (display "" BUT EXPECTED "")
	                     (write expect)
	                     (newline)
	                     #f)
	                  (else #t)))
                   (if (procedure? fun) (apply fun args) (car args)))))");
        }

        /// <summary>
        /// Run the given test and check that the result it true.
        /// </summary>
        /// <param name="test">The test program.</param>
        private void RunTest(string test)
        {
            SchemeObject res = this.ReadAndEvaluate(test);
            Assert.IsInstanceOfType(res, typeof(SchemeBoolean));
            Assert.IsTrue(((SchemeBoolean)res).Value, "Failed " + this.section);
        }

        /// <summary>
        /// Run a test and check the result.
        /// </summary>
        /// <param name="expected">The expected result.</param>
        /// <param name="label">The label to display.</param>
        /// <param name="expr">The expression to evaluate.</param>
        private void Run(string expected, string label, string expr)
        {
            SchemeObject res = this.ReadAndEvaluate(expr);
            string actual = res.ToString(true);
            Console.WriteLine("({0} {1}) ==> {2}", label, expected, actual);
            Assert.AreEqual(expected, actual, "Failed " + this.section);
        }

        /// <summary>
        /// Read a string and evaluate it.
        /// </summary>
        /// <param name="str">The string to read.</param>
        /// <returns>The value of the last expression.</returns>
        private SchemeObject ReadAndEvaluate(string str) 
        {
            using (var reader = new StringReader(str))
            {
                InputPort inp = InputPort.New(reader, (Interpreter)this.interpreter);
                SchemeObject last = EmptyList.Instance;
                while (true)
                {
                    SchemeObject x;
                    if ((x = inp.Read()) is Eof)
                    {
                        return last;
                    }

                    last = this.interpreter.Eval(x);
                }
            }
        }
    }
}
