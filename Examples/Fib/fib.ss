;; fib.ss
;; performance measurements

(define count 1)
(define (test f)
  (let ((t (second (time-call f count))))
    (display t)
    (display #\tab) (p f))
  )

(define fib
  (lambda (m)
    (if (< m 2) 
     	m
        (+ (fib (- m 1)) (fib (- m 2))))))
(define (fib-iter n)
  (do ((x1 0) (x2 1) (tmp 1) (i 1 (+ i 1)))
    ((> i n) x1)
     (set! tmp (+ x1 x2))
     (set! x1 x2)
     (set! x2 tmp)))

(define print-fib
  (lambda (m) 
    (do ((i 0 (+ i 1)))
      ((> i m) 'done)
      (p (fib-iter i)))))
(define run-fib
  (lambda (m) 
    (do ((i 0 (+ i 1)))
      ((>= i m) 'done)
      (test fib25))))
(define fib25 (lambda () (fib 25)))
(run-fib 1)
;(p (fib-iter 1000))
;(debug)


