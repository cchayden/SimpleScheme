;; test async clr methods

;; define WebRequest and WebResponse classes
(define WebRequest "System.Net.WebRequest, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define WebResponse "System.Net.WebResponse, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")

;; define CLR method accessors
(define WebRequest.Create (method WebRequest "Create" "string"))
(define req.GetResponse (method-async WebRequest "GetResponse"))
(define resp.GetContentLength (method WebResponse "get_ContentLength"))
(define resp.GetResponseStream (method WebResponse "GetResponseStream"))

; get content length asynchronously
(define (get-content-length uri)
  (letrec 
    ((resp (req.GetResponse (WebRequest.Create uri)))
     (stream #f))
    (display (string-append "Uri " uri " : " ))
    (p (resp.GetContentLength resp))
	(set! stream (resp.GetResponseStream resp))
  ))

; sequential
;(for-each get-content-length '("http://microsoft.com" "http://live.com" "http://wintellect.com"))

; parallel
(get-content-length "http://microsoft.com")  
(get-content-length "http://live.com")  
(get-content-length "http://wintellect.com")
 

(p "loaded")
