;;; FIBFP -- Computes fib(35) using floating point

(define fl<? <)
(define fl- -)
(define fl+ +)

(define (fibfp n)
  (if (fl<? n 2.)
    n
    (fl+ (fibfp (fl- n 1.))
         (fibfp (fl- n 2.)))))

(define (main inport)
  (let* ((count (read inport))
         (input (read inport))
         (output (read inport))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fibfp"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fibfp (hide count input)))
     (lambda (result) (= result output)))))
