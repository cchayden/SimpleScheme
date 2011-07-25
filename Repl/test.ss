(define return 0)
(define count 0)
(define x '(1 2 3 4 5 6 7 8))
(define show 
  (lambda (x) 
    (set! count (+ 1 count))
    (if (= x 4)
        (call/cc (lambda (cont) (set! return cont) 4.5))
        x)))
(display (map show x))    ;   => (1 2 3 4.5 5 6 7 8)
;(display count)(newline)
;(return 22)  ; => (1 2 3 22 5 6 7 8)


