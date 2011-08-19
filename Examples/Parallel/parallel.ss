;; test parallel

;; methods
(define sleep-caller ((method "Parallel,Parallel" "CreateAsync")))
(define async-sleep (method-async "Parallel+SleepCaller,Parallel" "Invoke" "int"))
(define (sleep duration) (async-sleep sleep-caller duration))

(define res1 #f)
(define res2 #f)
(define res3 #f)
(define (test)
    (set! res1 #f) 
    (set! res2 #f) 
    (set! res3 #f) 
    (parallel (begin (sleep 100) (set! res1 #t))
              (begin (sleep 100) (set! res2 #t))
              (begin (sleep 100) (set! res3 #t)))
    (list res1 res2 res3)
)

;(trace-on)
;(counters-on)
