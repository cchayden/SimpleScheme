;; perf.ss
;; performance measurements

(define (test f)
  (display f) (display " ")
  (p (first (second (time-call f 1000000)))))

(define (double x) (* 2 x))
(define x 10)

(test (lambda () ))
(test (lambda () 10))
(test (lambda () #\a))
(test (lambda () #\a))
(test (lambda () #\a))
(test (lambda () #\a))
(test (lambda () "xxx"))
(test (lambda () '()))
(test (lambda () x))
(test (lambda () (begin 10)))
(test (lambda () (define x 10)))
(test (lambda () (set! x 10)))
(test (lambda () (if #t 10 11)))
(test (lambda () (make-vector 10)))
(test (lambda () (let ((x 10)) x)))
(test (lambda () (double 10)))
