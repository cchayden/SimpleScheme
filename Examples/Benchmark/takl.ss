;;; TAKL -- The TAKeuchi function using lists as counters.
 
(define (listn n)
  (if (= n 0)
    '()
    (cons n (listn (- n 1)))))
 
(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))
 
(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))
 
(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))
 
(define (main inport)
  (let* ((count (read inport))
         (input1 (read inport))
         (input2 (read inport))
         (input3 (read inport))
         (output (read inport))
         (s4 (number->string count))
         (s3 (number->string (length input3)))
         (s2 (number->string (length input2)))
         (s1 (number->string (length input1)))
         (name "takl"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (mas (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? (length result) output)))))