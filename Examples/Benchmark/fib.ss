;;; FIB -- A classic benchmark, computes fib(n) inefficiently.

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
  
(define (main inport)
  (let* ((count (read inport))
         (input (read inport))
         (output (read inport))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fib"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fib (hide count input)))
     (lambda (result) (= result output)))))
