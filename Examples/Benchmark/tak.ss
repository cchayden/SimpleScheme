;;; TAK -- A vanilla version of the TAKeuchi function.
 
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (main inport)
  (let* ((count (read inport))
         (input1 (read inport))
         (input2 (read inport))
         (input3 (read inport))
         (output (read inport))
         (s4 (number->string count))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "tak"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (tak (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? result output)))))