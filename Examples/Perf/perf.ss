;; perf.ss
;; performance measurements

(define (test f)
  (display f) (display " ")
  (p (first (second (time-call f 1000000)))))

(define (double x) (* 2 x))
(define x 10)

(p 'base)
(test (lambda () ))

(p 'constants)
(test (lambda () 10))
(test (lambda () #\a))
(test (lambda () "xxx"))
(test (lambda () '()))

(p 'symbol)
(test (lambda () x))

(p 'syntax)
(test (lambda () (and 1)))
(test (lambda () (begin 10)))
(test (lambda () (case 10 ((10) 1)) ))
(test (lambda () (cond (#t 1))))
(test (lambda () (define x 10)))
(test (lambda () 
  (do ((i 0 (+ i 1))) ((= i 1)) 
      (+ i i))))
(test (lambda () (if #t 10 11)))
(test (lambda () (lambda ())))
(test (lambda () (let ((x 10)) x)))
(test (lambda () (let* ((x 10)) x)))
(test (lambda () (letrec ((x 10)) x)))
(test (lambda () (or 1)))
(test (lambda () (set! x 10)))

(p 'vector)
(test (lambda () (make-vector 10)))

(p 'number)
(test (lambda () (* 2 10)))

(p 'procedure)
(test (lambda () (double 10)))

(p 'args)
(test (lambda () (and)))
(test (lambda () (and 1)))
(test (lambda () (and 1 2)))
(test (lambda () (and 1 2 3)))
(test (lambda () (and 1 2 3 4)))


