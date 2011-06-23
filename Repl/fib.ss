;; fib.ss
;; fibonacci-based perf tests

(define fibonacci
  (lambda (n) 
    (let fib ((i n))
      (cond
        ((= i 0) 0)
        ((= i 1) 1)
        (else (+ (fib (- i 1)) (fib (- i 2))))))))

(display "fib 10: ")(display (time (fibonacci 10)))(newline)
(display "fib 15: ")(display (time (fibonacci 15)))(newline)
(display "fib 20: ")(display (time (fibonacci 20)))(newline)
(display "fib 25: ")(display (time (fibonacci 25)))(newline)
(exit)
