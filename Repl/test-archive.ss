;; count down -- take some time
(define count-down (lambda (x) 
    (let ((y (if (eq? x 0) x (count-down (- x 1)))))
	y)))
(define (cd1) (count-down 1000))

;; test synchronous clr methods
(define tc "Repl.TestClass, Repl")
(define x (method tc "TestMethod1"))
(define y (method tc "TestMethod2"))
(define c (new tc))

;; test async clr methods
(define req-class "System.Net.WebRequest, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define resp-class "System.Net.WebResponse, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define req-create (method req-class "Create" "string"))
(define req-get-response (method req-class "GetResponse"))
(define req-async-get-response (method-async req-class "GetResponse"))
(define resp-get-content-length (method resp-class "get_ContentLength"))

(begin 
  (define req (req-create "http://microsoft.com"))
  (define resp (req-async-get-response req))
  (display "content length: " )
  (p (resp-get-content-length resp)))

(p "loaded")
