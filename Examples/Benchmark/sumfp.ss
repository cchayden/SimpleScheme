;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

(define (run n)
  (let loop ((i n) (sum 0.))
    (if (< i 0.)
        sum
        (loop (- i 1.) (+ i sum)))))
 
(define (main inport)
  (let* ((count (read inport))
         (input1 (read inport))
         (output (read inport))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "sumfp"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (run (hide count input1)))
     (lambda (result) (equal? result output)))))