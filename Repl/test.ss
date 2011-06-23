(define (count-down1 n) (if (= n 0) 0 (count-down1 (- n 1))))
(define (count-down2 n) (if (= n 0) 0 (+ (count-down2 (- n 1)) 0)))
(p "loaded")
