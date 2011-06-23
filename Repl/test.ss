;; test async clr methods

;; define WebRequest and Webresponse classes
(define req-class "System.Net.WebRequest, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define resp-class "System.Net.WebResponse, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")

;; define WebRequest.Create method
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