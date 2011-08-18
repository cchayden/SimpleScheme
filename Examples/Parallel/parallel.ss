;; test async clr methods

;; methods
(define create-async (method "Parallel,Parallel" "CreateAsync"))
(define sleep-caller (create-async))
(define async-sleep (method-async "Parallel+TestSleepCaller,Parallel" "Invoke" "int"))
(define (sleep duration) (async-sleep sleep-caller duration))
(define delay (method "Parallel,Parallel" "TestSleep" "int"))

(define res1 #f)
(define res2 #f)
(define res3 #f)
(define test 
  (lambda ()
    (set! res1 #f) 
    (set! res2 #f) 
    (set! res3 #f) 
    (parallel (begin (sleep 100) (set! res1 #t))
              (begin (sleep 100) (set! res2 #t))
              (begin (sleep 100) (set! res3 #t)))
    (list res1 res2 res3)
))

;(trace-on)