;;; The following code is appended to all benchmarks.

;;; Given an integer and an object, returns the object
;;; without making it too easy for compilers to tell
;;; the object will be returned.

(define (hide r x) x)

;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; and a unary predicate that is true of the
;;; correct results the thunk may return,
;;; runs the benchmark for the number of specified iterations.
;;;
;;; Implementation-specific versions of this procedure may
;;; provide timings for the benchmark proper (without startup
;;; and compile time).

(define (run-r6rs-benchmark name count thunk ok?)
  (display "Running ")
  (display name)
  (newline)
  (time (let loop ((i 0)
             (result (if #f #f)))
    (cond ((< i count)
           (loop (+ i 1) (thunk)))
          ((ok? result)
           (list name count))
          (else
           (display "ERROR: returned incorrect result: ")
           (write result)
           (newline)
           result)))))

(define (main1 filename) 
  (let ((res (main (open-input-file filename)))
        (msec (lambda (res) (first (first (rest res)))))
        (mem (lambda (res) (first (second (rest res)))))
        (name (lambda (res) (first (first res))))
        (count (lambda (res) (second (first res)))))
    (list (name res) (count res) (msec res) (mem res))))
