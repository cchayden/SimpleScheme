(define (build-par n par) 
  (if (= n 0) par
    (cons (list 'begin '(p 100) n)
      (build-par (- n 1) par))))
(define (build-p n) (cons 'parallel (build-par n '())))


