(define (count-down n) 
  (cond ((= n 0) 'done)
        (else (count-down (- n 1)))))
(define (time-it)
  (p (time (count-down 1000)))
  (p (time (count-down 1000000))))

(time-it)