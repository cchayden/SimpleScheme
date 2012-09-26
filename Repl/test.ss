;(trace-on)
;; fib.ss
;; fibonacci-based perf tests

(counters-on)
(define fibonacci
  (lambda (n) 
    (let fib ((i n))
      (cond
        ((= i 0) 0)
        ((= i 1) 1)
        (else (+ (fib (- i 1)) (fib (- i 2))))))))

(display "fib 10: ")(display (time (fibonacci 10)))(newline)
(dump-counters)
