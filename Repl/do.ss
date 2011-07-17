;; do.ss
;; do-based perf tests

(define do-test
  (lambda (n fun) 
    (do ((i 0 (+ i 1))) ((= i n)) 
      (fun i i i i i i))))

; gives microseconds per iteration
(define test (lambda (n fun) 
  (/ (* 1000 (caadr (time (do-test n fun)))) n)))

;(counters-on)
;(trace-on)
(begin (display (test 1000 and))(newline))
;(trace-off)
;(counters-off)
;(dump-counters)
;(exit)
