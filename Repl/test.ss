(define count-down (lambda (x) 
    (let ((y (if (eq? x 0) x (count-down (- x 1)))))
	y)))
(define (cd1) (count-down 1000))
(define x (method "TestMethod1" "SimpleScheme.TestClass"))
(define y (method "TestMethod2" "SimpleScheme.TestClass"))
(define c (new "SimpleScheme.TestClass"))

(p "loaded")
