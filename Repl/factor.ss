;; factor.ss
;; factor numbers and time the

(reset-counters)

(define factor
  (lambda (n)
    (let f ((n n) (i 2))
      (cond
        ((> i n) '())
        ((integer? (/ n i)) (cons i (f (/ n i) i)))
        (else (f n (+ i 1)))))))
(define (test-factor n) (display "factor: ")(display n)(display " ")(display (factor n))(newline))						

(counters-on)
(do ((i 0 (+ i 1))) ((= i 10)) 
 (begin
  (test-factor 12)
  (test-factor 3628800)
  (test-factor 9239)
  (test-factor 9876543)
  (test-factor 105943)
 )
)
(counters-off)
(dump-counters)
;(exit)
