;; factor.ss
;; factor numbers

(reset-counters)

(define factor
  (lambda (n)
    (let f ((n n) (i 2))
      (cond
        ((> i n) '())
        ((integer? (/ n i)) (cons i (f (/ n i) i)))
        (else (f n (+ i 1)))))))				

(display "factor 12: ")(display (time (factor 12)))(newline)
(display "factor 3628800: ")(display (time (factor 3628800)))(newline)
(display "factor 9239: ")(display (time (factor 9239)))(newline)
(display "factor 9876543: ")(display (time (factor 9876543)))(newline)
(dump-counters)
(exit)
