// <copyright file="R4RSTest.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
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
    public class R4RsTest
    {
        /// <summary>
        /// A scheme interpreter, created for each test.
        /// </summary>
        private Scheme scheme;

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
            this.scheme = new Scheme();
        }

        /// <summary>
        /// Test that all symbol characters are supported.
        /// </summary>
        [TestMethod]
        public void IdentifierTest()
        {
            this.section = "2.1";
            const string Test = "'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)";
            object res = this.ReadAndEvaluate(Test);
            Assert.AreEqual(17, SchemeUtils.Length(res), "Failed " + this.section);
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
            string[] predicates = new[] { "boolean?", "char?",      "null?", "number?", "pair?",    "procedure?", "string?",    "symbol?", "vector?" };
            string[] examples1 = new[] { "#t",        "#\\a",       "'()",   "9739",    "'(test)",  "double",     @"""test""",  "'car",     "'#(a b c)" };
            string[] examples2 = new[] { "#f",        "#\\newline", "'()",   "-3252",   "'(t . t)", "car",        @"""""",      "'nil",     "'#()" };
            this.ReadAndEvaluate("(define double (lambda (x) (* 2 x)))");
            string[][] examples = { examples1, examples2 };
            foreach (string[] ex in examples)
            {
                for (int i = 0; i < predicates.Length; i++)
                {
                    for (int j = 0; j < examples1.Length; j++)
                    {
                        string test = String.Format("({0} {1})", predicates[i], ex[j]);
                        object res = this.ReadAndEvaluate(test);
                        Assert.AreEqual(i == j, res, "Failed " + this.section + " " + test);
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
            Assert.AreEqual(12.0, this.ReadAndEvaluate("((if #f + *) 3 4)"), "Failed " + this.section);
        }

        /// <summary>
        /// Test lambda
        /// </summary>
        [TestMethod]
        public void LambdaTest()
        {
            this.section = "4.1.4";
            Assert.AreEqual(8.0, this.ReadAndEvaluate("((lambda (x) (+ x x)) 4)"), "Failed " + this.section);
            this.ReadAndEvaluate("(define reverse-subtract (lambda (x y) (- y x)))");
            Assert.AreEqual(3.0, this.ReadAndEvaluate("(reverse-subtract 7 10)"), "Failed " + this.section);
            this.ReadAndEvaluate(
                @"(define add4
                    (let ((x 4))
                         (lambda (y) (+ x y))))");

            Assert.AreEqual(10.0, this.ReadAndEvaluate("(add4 6)"), "Failed 4.1.4");
            Assert.AreEqual("(3 4 5 6)", this.ReadAndEvaluate("((lambda x x) 3 4 5 6)").ToString(), "Failed " + this.section);
            Assert.AreEqual("(5 6)", this.ReadAndEvaluate("((lambda (x y . z) z) 3 4 5 6))").ToString(), "Failed " + this.section);
        }

        /// <summary>
        /// Test more if
        /// </summary>
        [TestMethod]
        public void IfMoreTest()
        {
            this.LoadTest("4.1.5");
            this.RunTest(@"(define x 2)
                           (test 3 'define (+ x 1))");
            this.RunTest(@"(set! x 4)
                           (test 5 'set! (+ x 1))");
        }

        /// <summary>
        /// Test define and set!
        /// </summary>
        [TestMethod]
        public void DefineSetTest()
        {
            this.LoadTest("4.1.6");
            this.RunTest("(test 'yes 'if (if (> 3 2) 'yes 'no))");
            this.RunTest("(test 'no 'if (if (> 2 3) 'yes 'no))");
            this.RunTest("(test '1 'if (if (> 3 2) (- 3 2) (+ 3 2)))");
        }

        /// <summary>
        /// Test conditionals, and, and or
        /// </summary>
        [TestMethod]
        public void ConditionalsTest()
        {
            this.LoadTest("4.2.1");
            this.RunTest(@"(test 'greater 'cond (cond ((> 3 2) 'greater)
			                  ((< 3 2) 'less)))");
            this.RunTest(@"(test 'equal 'cond (cond ((> 3 3) 'greater)
			                  ((< 3 3) 'less)
			                (else 'equal)))");
            this.RunTest(@"(test 2 'cond (cond ((assv 'b '((a 1) (b 2))) => cadr)
		                                 (else #f)))");
            this.RunTest(@"(test 'composite 'case (case (* 2 3)
			                                   ((2 3 5 7) 'prime)
			                                   ((1 4 6 8 9) 'composite)))");
            this.RunTest(@"(test 'consonant 'case (case (car '(c d))
			                      ((a e i o u) 'vowel)
			                      ((w y) 'semivowel)
			                      (else 'consonant)))");
            this.RunTest("(test #t 'and (and (= 2 2) (> 2 1)))");
            this.RunTest("(test #f 'and (and (= 2 2) (< 2 1)))");
            this.RunTest("(test '(f g) 'and (and 1 2 'c '(f g)))");
            this.RunTest("(test #t 'and (and))");
            this.RunTest("(test #t 'or (or (= 2 2) (> 2 1)))");
            this.RunTest("(test #t 'or (or (= 2 2) (< 2 1)))");
            this.RunTest("(test #f 'or (or #f #f #f))");
            this.RunTest("(test #f 'or (or))");
            this.RunTest("(test '(b c) 'or (or (memq 'b '(a b c)) (+ 3 0)))");
        }

        /// <summary>
        /// Test let and friends
        /// </summary>
        [TestMethod]
        public void LetTest()
        {
            this.LoadTest("4.2.2");
            this.RunTest("(test 6 'let (let ((x 2) (y 3)) (* x y)))");
            this.RunTest("(test 35 'let (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))");
            this.RunTest("(test 70 'let* (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))");
            this.RunTest(@"(test #t 'letrec (letrec ((even?
			               (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
			                 (odd?
			                   (lambda (n) (if (zero? n) #f (even? (- n 1))))))
		                            (even? 88)))");
            this.RunTest(@"(define x 34)
                           (test 5 'let (let ((x 3)) (define x 5) x))");
            this.RunTest("(test 34 'let x)");
            this.RunTest("(test 6 'let (let () (define x 6) x))");
            this.RunTest("(test 34 'let x)");
            this.RunTest("(test 34 'let (let ((x x)) x))");
            this.RunTest("(test 7 'let* (let* ((x 3)) (define x 7) x))");
            this.RunTest("(test 34 'let* x)");
            this.RunTest("(test 8 'let* (let* () (define x 8) x))");
            this.RunTest("(test 34 'let* x)");
            this.RunTest("(test 9 'letrec (letrec () (define x 9) x))");
            this.RunTest("(test 34 'letrec x)");
            this.RunTest("(test 10 'letrec (letrec ((x 3)) (define x 10) x))");
            this.RunTest("(test 34 'letrec x)");
        }

        /// <summary>
        /// Test begin
        /// </summary>
        [TestMethod]
        public void BeginTest()
        {
            this.LoadTest("4.2.3");
            this.RunTest(@"(define x 0)
                           (test 6 'begin (begin (set! x (begin (begin 5)))
		                     (begin ((begin +) (begin x) (begin (begin 1))))))");
        }

        /// <summary>
        /// Test loops
        /// </summary>
        [TestMethod]
        public void LoopTest()
        {
            this.LoadTest("4.2.4");

            this.RunTest(@"(test '#(0 1 2 3 4) 'do (do ((vec (make-vector 5))
			    (i 0 (+ i 1)))
			   ((= i 5) vec)
			 (vector-set! vec i i)))");

            this.RunTest(@"
            (test 25 'do (let ((x '(1 3 5 7 9)))
     	       (do ((x x (cdr x))
     		    (sum 0 (+ sum (car x))))
     		   ((null? x) sum))))");
            this.RunTest(@"
            (test 25 'do (let ((x '(1 3 5 7 9))
                   (sum 0))
     	       (do ((x x (cdr x)))
    		   ((null? x))
    		 (set! sum (+ sum (car x))))
    	       sum))");
            this.RunTest("(test 1 'let (let foo () 1))");
            this.RunTest(@"  
            (test '((6 1 3) (-5 -2)) 'let
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
       		     neg)))))");

            // TODO this does not pass
            // this.RunTest(@"(test -1 'let (let ((f -)) (let f ((n (f 1))) n)))");
        }

        /// <summary>
        /// Test quasi quote
        /// </summary>
        [TestMethod]
        public void QuasiQuoteTest()
        {
            this.LoadTest("4.2.6");
            this.RunTest(@"(test '(list 3 4) 'quasiquote `(list ,(+ 1 2) 4))");
            this.RunTest(@"(test '(list a (quote a)) 'quasiquote (let ((name 'a)) `(list ,name ',name)))");
            this.RunTest(@"(test '(a 3 4 5 6 b) 'quasiquote `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))");
            this.RunTest(@"(test '((foo 7) . cons)
	                           'quasiquote
	                               `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))");
            this.ReadAndEvaluate(@"(define (sqt x)
	                          (do ((i 0 (+ i 1)))
	                             ((> (* i i) x) (- i 1))))");

            this.RunTest(@"(test '#(10 5 2 4 3 8) 'quasiquote `#(10 5 ,(sqt 4) ,@(map sqt '(16 9)) 8))");
            this.RunTest(@"(test 5 'quasiquote `,(+ 2 3))");
            this.RunTest(@"(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
                           'quasiquote `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))");
            this.RunTest(@"(test '(a `(b ,x ,'y d) e) 'quasiquote
	                           (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)))
                                    (test '(list 3 4) 'quasiquote (quasiquote (list (unquote (+ 1 2)) 4)))");
            this.RunTest(@"(test '`(list ,(+ 1 2) 4) 'quasiquote '(quasiquote (list (unquote (+ 1 2)) 4)))");
        }

        /// <summary>
        /// Test begin some more
        /// </summary>
        [TestMethod]
        public void BeginMoreTest()
        {
            this.LoadTest("5.2.1");
            this.RunTest(@"(define (tprint x) #t)
                           (test #t 'tprint (tprint 56))");
            this.RunTest(@"(define add3 (lambda (x) (+ x 3)))
                           (test 6 'define (add3 3))");
            this.RunTest(@"(define first car)
                           (test 1 'define (first '(1 2)))");
            this.RunTest(@"(define foo (lambda () 9))
                           (test 9 'define (foo))");
            this.RunTest(@"(define foo foo)
                           (test 9 'define (foo))");
            this.RunTest(@"(define foo (let ((foo foo)) (lambda () (+ 1 (foo)))))
                           (test 10 'define (foo))");
            this.RunTest(@"(define old-+ +)
                           (begin (begin (begin)
	                            (begin (begin (begin) (define + (lambda (x y) (list y x)))
			                        (begin)))
	                            (begin))
                           (begin)
                          (begin (begin (begin) (test '(3 6) add3 6)
		                      (begin))))
                         (set! + old-+)
                         (test 9 add3 6)");
            this.ReadAndEvaluate(@"(begin)
                           (begin (begin))
                           (begin (begin (begin (begin))))");
        }

        /// <summary>
        /// Test define some more
        /// </summary>
        [TestMethod]
        public void DefineMoreTest()
        {
            this.LoadTest("5.2.2");
            this.RunTest(@"(test 45 'define
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
	                  (begin (foo (+ x 3)))))");
            this.RunTest(@"(define x 34)
                           (define (foo) (define x 5) x)
                           (test 5 foo)");
            this.RunTest(@"(test 34 'define x)");
            this.RunTest(@"(define foo (lambda () (define x 5) x))
                           (test 5 foo)");
            this.RunTest(@"(test 34 'define x)");
            this.RunTest(@"(define (foo x) ((lambda () (define x 5) x)) x)
                           (test 88 foo 88)");
            this.RunTest(@"(test 4 foo 4)");
            this.RunTest(@"(test 34 'define x)");
            this.RunTest(@"(test 99 'internal-define (letrec ((foo (lambda (arg)
					  (or arg (and (procedure? foo)
						       (foo 99))))))
			    (define bar (foo #f))
			    (foo #f)))");
            this.RunTest(@"(test 77 'internal-define (letrec ((foo 77)
				   (bar #f)
				   (retfoo (lambda () foo)))
			    (define baz (retfoo))
			    (retfoo)))");
        }

        /// <summary>
        /// Test booleans
        /// </summary>
        [TestMethod]
        public void NotTest()
        {
            this.LoadTest("6.1");
            this.RunTest(@"(test #f not #t)");
            this.RunTest(@"(test #f not 3)");
            this.RunTest(@"(test #f not (list 3))");
            this.RunTest(@"(test #t not #f)");
            this.RunTest(@"(test #f not '())");
            this.RunTest(@"(test #f not (list))");
            this.RunTest(@"(test #f not 'nil)");
            this.RunTest(@"(test #t boolean? #f)");
            this.RunTest(@"(test #f boolean? 0)");
            this.RunTest(@"(test #f boolean? '())");
        }

        /// <summary>
        /// Test equivalence predicates
        /// </summary>
        [TestMethod]
        public void EquivalenceTest()
        {
            this.LoadTest("6.2");
            this.RunTest(@"(test #t eqv? 'a 'a)");
            this.RunTest(@"(test #f eqv? 'a 'b)");
            this.RunTest(@"(test #t eqv? 2 2)");
            this.RunTest(@"(test #t eqv? '() '())");
            this.RunTest(@"(test #t eqv? '10000 '10000)");
            this.RunTest(@"(test #f eqv? (cons 1 2)(cons 1 2))");
            this.RunTest(@"(test #f eqv? (lambda () 1) (lambda () 2))");
            this.RunTest(@"(test #f eqv? #f 'nil)");
            this.RunTest(@"(let ((p (lambda (x) x)))
                                      (test #t eqv? p p))");
            this.RunTest(@"(define gen-counter
                           (lambda ()
                              (let ((n 0))
                                 (lambda () (set! n (+ n 1)) n))))
                                      (let ((g (gen-counter))) (test #t eqv? g g))");
            this.RunTest(@"(test #f eqv? (gen-counter) (gen-counter))");
            this.RunTest(@"(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	                                (g (lambda () (if (eqv? f g) 'g 'both))))
                            (test #f eqv? f g))");

            this.RunTest(@"(test #t eq? 'a 'a)");
            this.RunTest(@"(test #f eq? (list 'a) (list 'a))");
            this.RunTest(@"(test #t eq? '() '())");
            this.RunTest(@"(test #t eq? car car)");
            this.RunTest(@"(let ((x '(a))) (test #t eq? x x))");
            this.RunTest(@"(let ((x '#())) (test #t eq? x x))");
            this.RunTest(@"(let ((x (lambda (x) x))) (test #t eq? x x))");

            this.ReadAndEvaluate(@"(define test-eq?-eqv?-agreement
                                      (lambda (obj1 obj2)
                                        (cond ((eq? (eq? obj1 obj2) (eqv? obj1 obj2)))
	                                     (else #f))))");
            this.RunTest(@"(test-eq?-eqv?-agreement '#f '#f)");
            this.RunTest(@"(test-eq?-eqv?-agreement '#t '#t)");
            this.RunTest(@"(test-eq?-eqv?-agreement '#t '#f)");
            this.RunTest(@"(test-eq?-eqv?-agreement '(a) '(a))");
            this.RunTest(@"(test-eq?-eqv?-agreement '(a) '(b))");
            this.RunTest(@"(test-eq?-eqv?-agreement car car)");
            this.RunTest(@"(test-eq?-eqv?-agreement car cdr)");
            this.RunTest(@"(test-eq?-eqv?-agreement (list 'a) (list 'a))");
            this.RunTest(@"(test-eq?-eqv?-agreement (list 'a) (list 'b))");
            this.RunTest(@"(test-eq?-eqv?-agreement '#(a) '#(a))");
            this.RunTest(@"(test-eq?-eqv?-agreement '#(a) '#(b))");
            this.RunTest(@"(test-eq?-eqv?-agreement ""abc"" ""abc"")");
            this.RunTest(@"(test-eq?-eqv?-agreement ""abc"" ""abz"")");

            this.RunTest(@"(test #t equal? 'a 'a)");
            this.RunTest(@"(test #t equal? '(a) '(a))");
            this.RunTest(@"(test #t equal? '(a (b) c) '(a (b) c))");
            this.RunTest(@"(test #t equal? ""abc"" ""abc"")");
            this.RunTest(@"(test #t equal? 2 2)");
            this.RunTest(@"(test #t equal? (make-vector 5 'a) (make-vector 5 'a))");
        }

        /// <summary>
        /// Test pairs and lists
        /// </summary>
        [TestMethod]
        public void PairListTest()
        {
            this.LoadTest("6.3");
            this.RunTest(@"(test '(a b c d e) 'dot '(a . (b . (c . (d . (e . ()))))))");
            this.RunTest(@"(define x (list 'a 'b 'c))
                           (define y x)
                              (and list? (test #t list? y))");
            this.RunTest(@"(set-cdr! x 4)
                           (test '(a . 4) 'set-cdr! x)");
            this.RunTest(@"(test #t eqv? x y)");
            this.RunTest(@"(test '(a b c . d) 'dot '(a . (b . (c . d))))");
            this.RunTest(@"(and list? (test #f list? y))");
            this.RunTest(@"(and list? (let ((x (list 'a))) (set-cdr! x x) (test #f 'list? (list? x))))");
            this.RunTest(@"(test #t pair? '(a . b))");
            this.RunTest(@"(test #t pair? '(a . 1))");
            this.RunTest(@"(test #t pair? '(a b c))");
            this.RunTest(@"(test #f pair? '())");
            this.RunTest(@"(test #f pair? '#(a b))");
            this.RunTest(@"(test '(a) cons 'a '())");
            this.RunTest(@"(test '((a) b c d) cons '(a) '(b c d))");
            this.RunTest(@"(test '(""a"" b c) cons ""a"" '(b c))");
            this.RunTest(@"(test '(a . 3) cons 'a 3)");
            this.RunTest(@"(test '((a b) . c) cons '(a b) 'c)");

            this.RunTest(@"(test 'a car '(a b c))");
            this.RunTest(@"(test '(a) car '((a) b c d))");
            this.RunTest(@"(test 1 car '(1 . 2))");

            this.RunTest(@"(test '(b c d) cdr '((a) b c d))");
            this.RunTest(@"(test 2 cdr '(1 . 2))");

            this.RunTest(@"(test '(a 7 c) list 'a (+ 3 4) 'c)");
            this.RunTest(@"(test '() list)");

            this.RunTest(@"(test 3 length '(a b c))");
            this.RunTest(@"(test 3 length '(a (b) (c d e)))");
            this.RunTest(@"(test 0 length '())");

            this.RunTest(@"(test '(x y) append '(x) '(y))");
            this.RunTest(@"(test '(a b c d) append '(a) '(b c d))");
            this.RunTest(@"(test '(a (b) (c)) append '(a (b)) '((c)))");
            this.RunTest(@"(test '() append)");
            this.RunTest(@"(test '(a b c . d) append '(a b) '(c . d))");
            this.RunTest(@"(test 'a append '() 'a)");

            this.RunTest(@"(test '(c b a) reverse '(a b c))");
            this.RunTest(@"(test '((e (f)) d (b c) a) reverse '(a (b c) d (e (f))))");

            this.RunTest(@"(test 'c list-ref '(a b c d) 2)");

            this.RunTest(@"(define e '((a 1) (b 2) (c 3)))
                           (test '(a 1) assq 'a e)");
            this.RunTest(@"(test '(b 2) assq 'b e)");
            this.RunTest(@"(test #f assq 'd e)");
            this.RunTest(@"(test #f assq (list 'a) '(((a)) ((b)) ((c))))");
            this.RunTest(@"(test '((a)) assoc (list 'a) '(((a)) ((b)) ((c))))");
            this.RunTest(@"(test '(5 7) assv 5 '((2 3) (5 7) (11 13)))");
        }

        /// <summary>
        /// Test symbols
        /// </summary>
        [TestMethod]
        public void SymbolTest()
        {
            this.LoadTest("6.4");
            this.RunTest(@"(test #t symbol? 'foo)");
            this.RunTest(@"(test #t symbol? (car '(a b)))");
            this.RunTest(@"(test #f symbol? ""bar"")");
            this.RunTest(@"(test #t symbol? 'nil)");
            this.RunTest(@"(test #f symbol? '())");
            this.RunTest(@"(test #f symbol? #f)");
            this.ReadAndEvaluate(@"(define char-standard-case char-upcase)
                                      (if (string=? (symbol->string 'A) ""a"")
                                        (set! char-standard-case char-downcase))");
            this.RunTest(@"(test #t 'standard-case
                             (string=? (symbol->string 'a) (symbol->string 'A)))");
            this.RunTest(@"(test #t 'standard-case
                             (or (string=? (symbol->string 'a) ""A"")
	                             (string=? (symbol->string 'A) ""a"")))");
            this.ReadAndEvaluate(@"(define (str-copy s)
                                      (let ((v (make-string (string-length s))))
                                        (do ((i (- (string-length v) 1) (- i 1)))
	                                    ((< i 0) v)
                                          (string-set! v i (string-ref s i)))))
                                   (define (string-standard-case s)
                                      (set! s (str-copy s))
                                      (do ((i 0 (+ 1 i))
                                        (sl (string-length s)))
                                      ((>= i sl) s)
                                      (string-set! s i (char-standard-case (string-ref s i)))))");
            this.RunTest(@"(test (string-standard-case ""flying-fish"") symbol->string 'flying-fish)");
            this.RunTest(@"(test (string-standard-case ""martin"") symbol->string 'Martin)");
            this.RunTest(@"(test ""Malvina"" symbol->string (string->symbol ""Malvina""))");
            this.RunTest(@"(test #t 'standard-case (eq? 'a 'A))");

            this.ReadAndEvaluate(@"(define x (string #\a #\b))
                                   (define y (string->symbol x))
                                   (string-set! x 0 #\c)");
            this.RunTest(@"(test ""cb"" 'string-set! x)");
            this.RunTest(@"(test ""ab"" symbol->string y)");
            this.RunTest(@"(test y string->symbol ""ab"")");
            this.RunTest(@"(test #t eq? 'mISSISSIppi 'mississippi)");
            this.RunTest(@"(test #f 'string->symbol (eq? 'bitBlt (string->symbol ""bitBlt"")))");
            this.RunTest(@"(test 'JollyWog string->symbol (symbol->string 'JollyWog))");
        }

        /// <summary>
        /// Test numbers
        /// </summary>
        [TestMethod]
        public void NumberTest()
        {
            this.LoadTest("6.5.5");
            this.RunTest("(test #t number? 3)");

            // no complex or rational
            this.RunTest("(test #t real? 3)");
            this.RunTest("(test #t integer? 3)");

            this.RunTest("(test #t exact? 3)");
            this.RunTest("(test #f inexact? 3)");

            this.RunTest("(test 1 expt 0 0)");
            this.RunTest("(test 0 expt 0 1)");
            this.RunTest("(test 0 expt 0 256)");
            this.RunTest("(test 0 expt 0 -255)");
            this.RunTest("(test 1 expt -1 256)");
            this.RunTest("(test -1 expt -1 255)");
            this.RunTest("(test 1 expt -1 -256)");
            this.RunTest("(test -1 expt -1 -255)");
            this.RunTest("(test 1 expt 256 0)");
            this.RunTest("(test 1 expt -256 0)");
            this.RunTest("(test 256 expt 256 1)");
            this.RunTest("(test -256 expt -256 1)");
            this.RunTest("(test 8 expt 2 3)");
            this.RunTest("(test -8 expt -2 3)");
            this.RunTest("(test 9 expt 3 2)");
            this.RunTest("(test 9 expt -3 2)");

            this.RunTest("(test #t = 22 22 22)");
            this.RunTest("(test #t = 22 22)");
            this.RunTest("(test #f = 34 34 35)");
            this.RunTest("(test #f = 34 35)");
            this.RunTest("(test #t > 3 -6246)");
            this.RunTest("(test #f > 9 9 -2424)");
            this.RunTest("(test #t >= 3 -4 -6246)");
            this.RunTest("(test #t >= 9 9)");
            this.RunTest("(test #f >= 8 9)");
            this.RunTest("(test #t < -1 2 3 4 5 6 7 8)");
            this.RunTest("(test #f < -1 2 3 4 4 5 6 7)");
            this.RunTest("(test #t <= -1 2 3 4 5 6 7 8)");
            this.RunTest("(test #t <= -1 2 3 4 4 5 6 7)");
            this.RunTest("(test #f < 1 3 2)");
            this.RunTest("(test #f >= 1 3 2)");

            this.RunTest("(test #t zero? 0)");
            this.RunTest("(test #f zero? 1)");
            this.RunTest("(test #f zero? -1)");
            this.RunTest("(test #f zero? -100)");
            this.RunTest("(test #t positive? 4)");
            this.RunTest("(test #f positive? -4)");
            this.RunTest("(test #f positive? 0)");
            this.RunTest("(test #f negative? 4)");
            this.RunTest("(test #t negative? -4)");
            this.RunTest("(test #f negative? 0)");
            this.RunTest("(test #t odd? 3)");
            this.RunTest("(test #f odd? 2)");
            this.RunTest("(test #f odd? -4)");
            this.RunTest("(test #t odd? -1)");
            this.RunTest("(test #f even? 3)");
            this.RunTest("(test #t even? 2)");
            this.RunTest("(test #t even? -4)");
            this.RunTest("(test #f even? -1)");

            this.RunTest("(test 38 max 34 5 7 38 6)");
            this.RunTest("(test -24 min 3  5 5 330 4 -24)");

            this.RunTest("(test 7 + 3 4)");
            this.RunTest("(test '3 + 3)");
            this.RunTest("(test 0 +)");
            this.RunTest("(test 4 * 4)");
            this.RunTest("(test 1 *)");
            this.RunTest("(test 1 / 1)");
            this.RunTest("(test -1 / -1)");
            this.RunTest("(test 2 / 6 3)");
            this.RunTest("(test -3 / 6 -2)");
            this.RunTest("(test -3 / -6 2)");
            this.RunTest("(test 3 / -6 -2)");
            this.RunTest("(test -1 - 3 4)");
            this.RunTest("(test -3 - 3)");
            this.RunTest("(test 7 abs -7)");
            this.RunTest("(test 7 abs 7)");
            this.RunTest("(test 0 abs 0)");

            this.RunTest("(test 5 quotient 35 7)");
            this.RunTest("(test -5 quotient -35 7)");
            this.RunTest("(test -5 quotient 35 -7)");
            this.RunTest("(test 5 quotient -35 -7)");
            this.RunTest("(test 1 modulo 13 4)");
            this.RunTest("(test 1 remainder 13 4)");
            this.RunTest("(test 3 modulo -13 4)");
            this.RunTest("(test -1 remainder -13 4)");
            this.RunTest("(test -3 modulo 13 -4)");
            this.RunTest("(test 1 remainder 13 -4)");
            this.RunTest("(test -1 modulo -13 -4)");
            this.RunTest("(test -1 remainder -13 -4)");
            this.RunTest("(test 0 modulo 0 86400)");
            this.RunTest("(test 0 modulo 0 -86400)");
            this.ReadAndEvaluate(@"(define (divtest n1 n2)
	                                (= n1 (+ (* n2 (quotient n1 n2))
		                                 (remainder n1 n2))))");
            this.RunTest("(test #t divtest 238 9)");
            this.RunTest("(test #t divtest -238 9)");
            this.RunTest("(test #t divtest 238 -9)");
            this.RunTest("(test #t divtest -238 -9)");

            this.RunTest("(test 4 gcd 0 4)");
            this.RunTest("(test 4 gcd -4 0)");
            this.RunTest("(test 4 gcd 32 -36)");
            this.RunTest("(test 0 gcd)");
            this.RunTest("(test 288 lcm 32 -36)");
            this.RunTest("(test 1 lcm)");
        }

        /// <summary>
        /// Test numbers -- more
        /// </summary>
        [TestMethod]
        public void MoreNumberTest()
        {
            this.LoadTest("6.5.5");
            this.ReadAndEvaluate(@"(define (test-string->number str)
                                   (define ans (string->number str))
                                      (cond ((not ans) #t) ((number? ans) #t) (else ans)))");
            this.RunTest(@"(test #t test-string->number ""+#.#"")");
            this.RunTest(@"(test #t test-string->number ""-#.#"")");
            this.RunTest(@"(test #t test-string->number ""#.#"")");
            this.RunTest(@"(test #t test-string->number ""1/0"")");
            this.RunTest(@"(test #t test-string->number ""-1/0"")");
            this.RunTest(@"(test #t test-string->number ""0/0"")");
            this.RunTest(@"(test #t test-string->number ""#e"")");
            this.RunTest(@"(test #t test-string->number ""#"")");
        }

        // No inexact number support
        // No bignum support

        /// <summary>
        /// Test string to number
        /// </summary>
        [TestMethod]
        public void StringToNumberTest()
        {
            this.LoadTest("6.5.9");
            this.RunTest(@"(test ""0"" number->string 0)");
            this.RunTest(@"(test ""100"" number->string 100)");
            this.RunTest(@"(test ""100"" number->string 256 16)");
            this.RunTest(@"(test 100 string->number ""100"")");
            this.RunTest(@"(test 256 string->number ""100"" 16)");
            this.RunTest(@"(test #f string->number """")");
            this.RunTest(@"(test #f string->number ""."")");
            this.RunTest(@"(test #f string->number ""d"")");
            this.RunTest(@"(test #f string->number ""D"")");
            this.RunTest(@"(test #f string->number ""-"")");
            this.RunTest(@"(test #f string->number ""+"")");
            this.RunTest(@"(test #t 'string->number (or (not (string->number ""80000000"" 16))
			     (positive? (string->number ""80000000"" 16))))");
            this.RunTest(@"(test #t 'string->number (or (not (string->number ""-80000000"" 16))
			     (negative? (string->number ""-80000000"" 16))))");
        }

        /// <summary>
        /// Test characters
        /// </summary>
        [TestMethod]
        public void CharacterTest()
        {
            this.LoadTest("6.6");
            this.RunTest(@"(test #t eqv? '#\  #\Space)");
            this.RunTest(@"(test #t eqv? #\space '#\Space)");
            this.RunTest(@"(test #t char? #\a)");
            this.RunTest(@"(test #t char? #\()");
            this.RunTest(@"(test #t char? #\space)");
            this.RunTest(@"(test #t char? '#\newline)");

            this.RunTest(@"(test #f char=? #\A #\B)");
            this.RunTest(@"(test #f char=? #\a #\b)");
            this.RunTest(@"(test #f char=? #\9 #\0)");
            this.RunTest(@"(test #t char=? #\A #\A)");

            this.RunTest(@"(test #t char<? #\A #\B)");
            this.RunTest(@"(test #t char<? #\a #\b)");
            this.RunTest(@"(test #f char<? #\9 #\0)");
            this.RunTest(@"(test #f char<? #\A #\A)");

            this.RunTest(@"(test #f char>? #\A #\B)");
            this.RunTest(@"(test #f char>? #\a #\b)");
            this.RunTest(@"(test #t char>? #\9 #\0)");
            this.RunTest(@"(test #f char>? #\A #\A)");

            this.RunTest(@"(test #t char<=? #\A #\B)");
            this.RunTest(@"(test #t char<=? #\a #\b)");
            this.RunTest(@"(test #f char<=? #\9 #\0)");
            this.RunTest(@"(test #t char<=? #\A #\A)");

            this.RunTest(@"(test #f char>=? #\A #\B)");
            this.RunTest(@"(test #f char>=? #\a #\b)");
            this.RunTest(@"(test #t char>=? #\9 #\0)");
            this.RunTest(@"(test #t char>=? #\A #\A)");

            this.RunTest(@"(test #f char-ci=? #\A #\B)");
            this.RunTest(@"(test #f char-ci=? #\a #\B)");
            this.RunTest(@"(test #f char-ci=? #\A #\b)");
            this.RunTest(@"(test #f char-ci=? #\a #\b)");
            this.RunTest(@"(test #f char-ci=? #\9 #\0)");
            this.RunTest(@"(test #t char-ci=? #\A #\A)");
            this.RunTest(@"(test #t char-ci=? #\A #\a)");

            this.RunTest(@"(test #t char-ci<? #\A #\B)");
            this.RunTest(@"(test #t char-ci<? #\a #\B)");
            this.RunTest(@"(test #t char-ci<? #\A #\b)");
            this.RunTest(@"(test #t char-ci<? #\a #\b)");
            this.RunTest(@"(test #f char-ci<? #\9 #\0)");
            this.RunTest(@"(test #f char-ci<? #\A #\A)");
            this.RunTest(@"(test #f char-ci<? #\A #\a)");

            this.RunTest(@"(test #f char-ci>? #\A #\B)");
            this.RunTest(@"(test #f char-ci>? #\a #\B)");
            this.RunTest(@"(test #f char-ci>? #\A #\b)");
            this.RunTest(@"(test #f char-ci>? #\a #\b)");
            this.RunTest(@"(test #t char-ci>? #\9 #\0)");
            this.RunTest(@"(test #f char-ci>? #\A #\A)");
            this.RunTest(@"(test #f char-ci>? #\A #\a)");

            this.RunTest(@"(test #t char-ci<=? #\A #\B)");
            this.RunTest(@"(test #t char-ci<=? #\a #\B)");
            this.RunTest(@"(test #t char-ci<=? #\A #\b)");
            this.RunTest(@"(test #t char-ci<=? #\a #\b)");
            this.RunTest(@"(test #f char-ci<=? #\9 #\0)");
            this.RunTest(@"(test #t char-ci<=? #\A #\A)");
            this.RunTest(@"(test #t char-ci<=? #\A #\a)");

            this.RunTest(@"(test #f char-ci>=? #\A #\B)");
            this.RunTest(@"(test #f char-ci>=? #\a #\B)");
            this.RunTest(@"(test #f char-ci>=? #\A #\b)");
            this.RunTest(@"(test #f char-ci>=? #\a #\b)");
            this.RunTest(@"(test #t char-ci>=? #\9 #\0)");
            this.RunTest(@"(test #t char-ci>=? #\A #\A)");
            this.RunTest(@"(test #t char-ci>=? #\A #\a)");

            this.RunTest(@"(test #t char-alphabetic? #\a)");
            this.RunTest(@"(test #t char-alphabetic? #\A)");
            this.RunTest(@"(test #t char-alphabetic? #\z)");
            this.RunTest(@"(test #t char-alphabetic? #\Z)");
            this.RunTest(@"(test #f char-alphabetic? #\0)");
            this.RunTest(@"(test #f char-alphabetic? #\9)");
            this.RunTest(@"(test #f char-alphabetic? #\space)");
            this.RunTest(@"(test #f char-alphabetic? #\;)");

            this.RunTest(@"(test #f char-numeric? #\a)");
            this.RunTest(@"(test #f char-numeric? #\A)");
            this.RunTest(@"(test #f char-numeric? #\z)");
            this.RunTest(@"(test #f char-numeric? #\Z)");
            this.RunTest(@"(test #t char-numeric? #\0)");
            this.RunTest(@"(test #t char-numeric? #\9)");
            this.RunTest(@"(test #f char-numeric? #\space)");
            this.RunTest(@"(test #f char-numeric? #\;)");

            this.RunTest(@"(test #f char-whitespace? #\a)");
            this.RunTest(@"(test #f char-whitespace? #\A)");
            this.RunTest(@"(test #f char-whitespace? #\z)");
            this.RunTest(@"(test #f char-whitespace? #\Z)");
            this.RunTest(@"(test #f char-whitespace? #\0)");
            this.RunTest(@"(test #f char-whitespace? #\9)");
            this.RunTest(@"(test #t char-whitespace? #\space)");
            this.RunTest(@"(test #f char-whitespace? #\;)");

            this.RunTest(@"(test #f char-upper-case? #\0)");
            this.RunTest(@"(test #f char-upper-case? #\9)");
            this.RunTest(@"(test #f char-upper-case? #\space)");
            this.RunTest(@"(test #f char-upper-case? #\;)");

            this.RunTest(@"(test #f char-lower-case? #\0)");
            this.RunTest(@"(test #f char-lower-case? #\9)");
            this.RunTest(@"(test #f char-lower-case? #\space)");
            this.RunTest(@"(test #f char-lower-case? #\;)");

            this.RunTest(@"(test #\. integer->char (char->integer #\.))");
            this.RunTest(@"(test #\A integer->char (char->integer #\A))");
            this.RunTest(@"(test #\a integer->char (char->integer #\a))");
            this.RunTest(@"(test #\A char-upcase #\A)");
            this.RunTest(@"(test #\A char-upcase #\a)");
            this.RunTest(@"(test #\a char-downcase #\A)");
            this.RunTest(@"(test #\a char-downcase #\a)");
        }

        /// <summary>
        /// Test strings
        /// </summary>
        [TestMethod]
        public void StringTest()
        {
            this.LoadTest("6.7");
            this.RunTest(@"(test #t string? ""The word \""recursion\\\"" has many meanings."")");
            this.RunTest(@"(test #t string? """")");
            this.ReadAndEvaluate(@"(define f (make-string 3 #\*))
                               (test ""?**"" 'string-set! (begin (string-set! f 0 #\?) f))");
            this.RunTest(@"(test ""abc"" string #\a #\b #\c)");

            this.RunTest(@"(test """" string)");
            this.RunTest(@"(test 3 string-length ""abc"")");
            this.RunTest(@"(test #\a string-ref ""abc"" 0)");
            this.RunTest(@"(test #\c string-ref ""abc"" 2)");
            this.RunTest(@"(test 0 string-length """")");
            this.RunTest(@"(test """" substring ""ab"" 0 0)");
            this.RunTest(@"(test """" substring ""ab"" 1 1)");
            this.RunTest(@"(test """" substring ""ab"" 2 2)");
            this.RunTest(@"(test ""a"" substring ""ab"" 0 1)");
            this.RunTest(@"(test ""b"" substring ""ab"" 1 2)");
            this.RunTest(@"(test ""ab"" substring ""ab"" 0 2)");

            this.RunTest(@"(test ""foobar"" string-append ""foo"" ""bar"")");
            this.RunTest(@"(test ""foo"" string-append ""foo"")");
            this.RunTest(@"(test ""foo"" string-append ""foo"" """")");
            this.RunTest(@"(test ""foo"" string-append """" ""foo"")");
            this.RunTest(@"(test """" string-append)");
            this.RunTest(@"(test """" make-string 0)");
            this.RunTest(@"(test #t string=? """" """")");
            this.RunTest(@"(test #f string<? """" """")");
            this.RunTest(@"(test #f string>? """" """")");
            this.RunTest(@"(test #t string<=? """" """")");
            this.RunTest(@"(test #t string>=? """" """")");
            this.RunTest(@"(test #t string-ci=? """" """")");
            this.RunTest(@"(test #f string-ci<? """" """")");
            this.RunTest(@"(test #f string-ci>? """" """")");
            this.RunTest(@"(test #t string-ci<=? """" """")");
            this.RunTest(@"(test #t string-ci>=? """" """")");

            this.RunTest(@"(test #f string=? ""A"" ""B"")");
            this.RunTest(@"(test #f string=? ""a"" ""b"")");
            this.RunTest(@"(test #f string=? ""9"" ""0"")");
            this.RunTest(@"(test #t string=? ""A"" ""A"")");
            this.RunTest(@"(test #t string<? ""A"" ""B"")");
            this.RunTest(@"(test #t string<? ""a"" ""b"")");
            this.RunTest(@"(test #f string<? ""9"" ""0"")");
            this.RunTest(@"(test #f string<? ""A"" ""A"")");

            this.RunTest(@"(test #f string>? ""A"" ""B"")");
            this.RunTest(@"(test #f string>? ""a"" ""b"")");
            this.RunTest(@"(test #t string>? ""9"" ""0"")");
            this.RunTest(@"(test #f string>? ""A"" ""A"")");

            this.RunTest(@"(test #t string<=? ""A"" ""B"")");
            this.RunTest(@"(test #t string<=? ""a"" ""b"")");
            this.RunTest(@"(test #f string<=? ""9"" ""0"")");
            this.RunTest(@"(test #t string<=? ""A"" ""A"")");

            this.RunTest(@"(test #f string>=? ""A"" ""B"")");
            this.RunTest(@"(test #f string>=? ""a"" ""b"")");
            this.RunTest(@"(test #t string>=? ""9"" ""0"")");
            this.RunTest(@"(test #t string>=? ""A"" ""A"")");

            this.RunTest(@"(test #f string-ci=? ""A"" ""B"")");
            this.RunTest(@"(test #f string-ci=? ""a"" ""B"")");
            this.RunTest(@"(test #f string-ci=? ""A"" ""b"")");
            this.RunTest(@"(test #f string-ci=? ""a"" ""b"")");
            this.RunTest(@"(test #f string-ci=? ""9"" ""0"")");
            this.RunTest(@"(test #t string-ci=? ""A"" ""A"")");
            this.RunTest(@"(test #t string-ci=? ""A"" ""a"")");

            this.RunTest(@"(test #t string-ci<? ""A"" ""B"")");
            this.RunTest(@"(test #t string-ci<? ""a"" ""B"")");
            this.RunTest(@"(test #t string-ci<? ""A"" ""b"")");
            this.RunTest(@"(test #t string-ci<? ""a"" ""b"")");
            this.RunTest(@"(test #f string-ci<? ""9"" ""0"")");
            this.RunTest(@"(test #f string-ci<? ""A"" ""A"")");
            this.RunTest(@"(test #f string-ci<? ""A"" ""a"")");

            this.RunTest(@"(test #f string-ci>? ""A"" ""B"")");
            this.RunTest(@"(test #f string-ci>? ""a"" ""B"")");
            this.RunTest(@"(test #f string-ci>? ""A"" ""b"")");
            this.RunTest(@"(test #f string-ci>? ""a"" ""b"")");
            this.RunTest(@"(test #t string-ci>? ""9"" ""0"")");
            this.RunTest(@"(test #f string-ci>? ""A"" ""A"")");
            this.RunTest(@"(test #f string-ci>? ""A"" ""a"")");

            this.RunTest(@"(test #t string-ci<=? ""A"" ""B"")");
            this.RunTest(@"(test #t string-ci<=? ""a"" ""B"")");
            this.RunTest(@"(test #t string-ci<=? ""A"" ""b"")");
            this.RunTest(@"(test #t string-ci<=? ""a"" ""b"")");
            this.RunTest(@"(test #f string-ci<=? ""9"" ""0"")");
            this.RunTest(@"(test #t string-ci<=? ""A"" ""A"")");
            this.RunTest(@"(test #t string-ci<=? ""A"" ""a"")");

            this.RunTest(@"(test #f string-ci>=? ""A"" ""B"")");
            this.RunTest(@"(test #f string-ci>=? ""a"" ""B"")");
            this.RunTest(@"(test #f string-ci>=? ""A"" ""b"")");
            this.RunTest(@"(test #f string-ci>=? ""a"" ""b"")");
            this.RunTest(@"(test #t string-ci>=? ""9"" ""0"")");
            this.RunTest(@"(test #t string-ci>=? ""A"" ""A"")");
            this.RunTest(@"(test #t string-ci>=? ""A"" ""a"")");
        }

        /// <summary>
        /// Test vectors
        /// </summary>
        [TestMethod]
        public void VectorTest()
        {
            this.LoadTest("6.8");
            this.RunTest(@"(test #t vector? '#(0 (2 2 2 2) ""Anna:""))");
            this.RunTest(@"(test #t vector? '#())");
            this.RunTest(@"(test '#(a b c) vector 'a 'b 'c)");
            this.RunTest(@"(test '#() vector)");
            this.RunTest(@"(test 3 vector-length '#(0 (2 2 2 2) ""Anna""))");
            this.RunTest(@"(test 0 vector-length '#())");
            this.RunTest(@"(test 8 vector-ref '#(1 1 2 3 5 8 13 21) 5)");
            this.RunTest(@"(test '#(0 (""Sue"" ""Sue"") ""Anna"") 'vector-set
	                           (let ((vec (vector 0 '(2 2 2 2) ""Anna"")))
	                                 (vector-set! vec 1 '(""Sue"" ""Sue""))
	                               vec))");
            this.RunTest(@"(test '#(hi hi) make-vector 2 'hi)");
            this.RunTest(@"(test '#() make-vector 0)");
            this.RunTest(@"(test '#() make-vector 0 'a) ");
        }

        /// <summary>
        /// Test control features
        /// </summary>
        [TestMethod]
        public void ControlFeaturesTest()
        {
            this.LoadTest("6.9");
            this.RunTest(@"(test #t procedure? car)");
            this.RunTest(@"(test #f procedure? 'car)");
            this.RunTest(@"(test #t procedure? (lambda (x) (* x x)))");
            this.RunTest(@"(test #f procedure? '(lambda (x) (* x x)))");
            this.RunTest(@"(test #t call-with-current-continuation procedure?)");
            this.RunTest(@"(test #t procedure? /)");
            this.RunTest(@"(test 7 apply + (list 3 4))");
            this.RunTest(@"(test 7 apply (lambda (a b) (+ a b)) (list 3 4))");
            this.RunTest(@"(test 17 apply + 10 (list 3 4))");
            this.RunTest(@"(test '() apply list '())");
            this.ReadAndEvaluate(@"(define compose (lambda (f g) (lambda args (f (apply g args)))))");
            this.RunTest(@"(test 30 (compose sqrt *) 12 75)");

            this.RunTest(@"(test '(b e h) map cadr '((a b) (d e) (g h)))");
            this.RunTest(@"(test '(5 7 9) map + '(1 2 3) '(4 5 6))");
            this.RunTest(@"(test '(1 2 3) map + '(1 2 3))");
            this.RunTest(@"(test '(1 2 3) map * '(1 2 3))");
            this.RunTest(@"(test '(-1 -2 -3) map - '(1 2 3))");
            this.RunTest(@"(test '#(0 1 4 9 16) 'for-each
                                (let ((v (make-vector 5)))
	                          (for-each (lambda (i) (vector-set! v i (* i i)))
		                          '(0 1 2 3 4))
	                          v))");
            this.RunTest(@"(test -3 call-with-current-continuation
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
            this.RunTest(@"(test 4 list-length '(1 2 3 4))");
            this.RunTest(@"(test #f list-length '(a b . c))");
            this.RunTest(@"(test '() map cadr '())");
        }

        /// <summary>
        /// Test call/cc
        /// </summary>
        [TestMethod]
        public void CallCCTest()
        {
            this.LoadTest("6.9");
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
            this.RunTest("(test #t leaf-eq? '(a (b (c))) '((a) b c))");
            this.RunTest("(test #f leaf-eq? '(a (b (c))) '((a) b c d))");
        }

        /// <summary>
        /// Test force and delay
        /// </summary>
        [TestMethod]
        public void ForceDelayTest()
        {
            this.LoadTest("6.9");
            this.RunTest(@"(test 3 'delay (force (delay (+ 1 2))))");
            this.RunTest(@"(test '(3 3) 'delay (let ((p (delay (+ 1 2))))
			   (list (force p) (force p))))");
            this.RunTest(@"(test 2 'delay (letrec ((a-stream
			       (letrec ((next (lambda (n)
					        (cons n (delay (next (+ n 1)))))))
			         (next 0)))
			      (head car)
			      (tail (lambda (stream) (force (cdr stream)))))
		        (head (tail (tail a-stream)))))");
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
            this.RunTest(@"(test 3 'force
	            (letrec ((p (delay (if c 3 (begin (set! c #t) (+ (force p) 1)))))
		             (c #f))
	              (force p)))");
        }

        /// <summary>
        /// Test input port
        /// </summary>
        [TestMethod]
        public void InputPortTest()
        {
            new StreamWriter("temp.scm").Close();   // create a file
            this.LoadTest("6.10.1");
            this.RunTest(@"(test #t input-port? (current-input-port))");
            this.RunTest(@"(test #t output-port? (current-output-port))");
            this.RunTest(@"(test #t call-with-input-file ""temp.scm"" input-port?)");
            this.RunTest(@"(define this-file (open-input-file ""temp.scm""))
                               (test #t input-port? this-file)");
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
            this.LoadTest("6.10.2");
            this.ReadAndEvaluate(@"(define this-file (open-input-file ""temp.scm""))");
            this.RunTest(@"(test #\; peek-char this-file)");
            this.RunTest(@"(test #\; peek-char this-file)");
            this.RunTest(@"(test #\; read-char this-file)");
            this.RunTest(@"(test '(define cur-section '()) read this-file)");
            this.RunTest(@"(test #\( peek-char this-file)");
            this.RunTest(@"(test '(define errs '()) read this-file)");
            this.RunTest(@"(close-input-port this-file)");
            this.RunTest(@"(close-input-port this-file)");
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
            this.LoadTest("6.10.3");
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
                (close-input-port test-file))
                (define write-test-obj
                 '(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c)))
             (define load-test-obj
                 (list 'define 'foo (list 'quote write-test-obj)))");
            this.RunTest(@"(test #t call-with-output-file
                         ""tmp1""
                         (lambda (test-file)
	                        (write-char #\; test-file)
	                        (display #\; test-file)
	                        (display "";"" test-file)
	                        (write write-test-obj test-file)
	                        (newline test-file)
	                        (write load-test-obj test-file)
	                        (output-port? test-file)))");
            this.RunTest(@"(check-test-file ""tmp1"")");

            this.ReadAndEvaluate(@"(define test-file (open-output-file ""tmp2""))
                              (write-char #\; test-file)
                              (display #\; test-file)
                              (display "";"" test-file)
                              (write write-test-obj test-file)
                              (newline test-file)
                              (write load-test-obj test-file)");
            this.RunTest(@"(test #t output-port? test-file)");
            this.RunTest(@"(close-output-port test-file)
                               (check-test-file ""tmp2"")");
            Assert.IsNull(this.ReadAndEvaluate("errs"));
        }

        /// <summary>
        /// Test new to r4
        /// </summary>
        [TestMethod]
        public void R4Test()
        {
            this.LoadTest("6.7");
            this.RunTest(@"(test '(#\P #\space #\l) string->list ""P l"")");
            this.RunTest(@"(test '() string->list """")");
            this.RunTest(@"(test ""1\\\"""" list->string '(#\1 #\\ #\""))");
            this.RunTest(@"(test """" list->string '())");

            this.section = "6.8";
            this.RunTest(@"(test '(dah dah didah) vector->list '#(dah dah didah))");
            this.RunTest(@"(test '() vector->list '#())");
            this.RunTest(@"(test '#(dididit dah) list->vector '(dididit dah))");
            this.RunTest(@"(test '#() list->vector '())");

            this.section = "6.10.4";
            TextWriter writer = new StreamWriter("tmp1");
            writer.WriteLine(@";;;(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c))
                     (define foo '(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c)))");
            writer.Close();
            this.RunTest(@"(load ""tmp1"")");
            this.ReadAndEvaluate(@"(define write-test-obj
                 '(#t #f a () 9739 -3 . #((test) ""te \"" \"" st"" """" test #() b c)))");
            this.RunTest(@"(test write-test-obj 'load foo)");
        }

        /// <summary>
        /// Load the test function in.
        /// </summary>
        /// <param name="sec">The test section.</param>
        private void LoadTest(string sec)
        {
            this.section = sec;
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
            Assert.AreEqual(true, this.ReadAndEvaluate(test), "Failed " + this.section);
        }

        /// <summary>
        /// Read a string and evaluate it.
        /// </summary>
        /// <param name="str">The string to read.</param>
        /// <returns>The value of the last expression.</returns>
        private object ReadAndEvaluate(string str) 
        {
            using (StringReader reader = new StringReader(str))
            {
                InputPort inp = new InputPort(reader);
                object last = null;
                while (true)
                {
                    object x;
                    if (InputPort.IsEOF(x = inp.Read()))
                    {
                        return last;
                    }

                    last = this.scheme.Eval(x);
                }
            }
        }
    }
}
