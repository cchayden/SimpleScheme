;; test async clr methods

;; define types
(define WebRequest "System.Net.WebRequest, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define WebResponse "System.Net.WebResponse, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define Stream "System.IO.Stream")
(define Encoding "System.Text.Encoding")
(define Decoder "System.Text.Decoder")

;; define .NET methods
(define WebRequest.Create (method WebRequest "Create" "string"))
(define async #t)
(if async 
  (begin 
    (define req.GetResponse (method-async WebRequest "GetResponse"))
    (define stream.ReadStream (method-async Stream "Read" "byte[]" "int" "int"))
  )
  (begin 
    (define req.GetResponse (method WebRequest "GetResponse"))
    (define stream.ReadStream (method Stream "Read" "byte[]" "int" "int"))
  ))
(define resp.GetContentLength (method WebResponse "get_ContentLength"))
(define resp.GetResponseStream (method WebResponse "GetResponseStream"))
(define utf8 (method Encoding "get_UTF8"))
(define get-decoder (method Encoding "GetDecoder"))
(define stream.Decode (get-decoder (utf8)))
(define decoder.GetChars (method Decoder "GetChars" "byte[]" "int" "int" "char[]" "int"))


; get content asynchronously
(define (get-content uri)
  (let*
    ((resp (req.GetResponse (WebRequest.Create uri)))
     (stream (resp.GetResponseStream resp))
     (out-string "")
     (out '()))
     ; function to decode byte array into string
     ; return a string
	 (define (decode buffer len)
	   (let ((char-buffer (make-string 256)))
		 (decoder.GetChars stream.Decode buffer 0 len char-buffer 0)
		 (substring char-buffer 0 len)
	 ))
     ; read a buffer of bytes from the stream and store into out-string
     ; return the number of bytes read
	 (define (read-buffer)
		(let* 
          ((buffer (new-array "byte" 256)) 
		   (len (stream.ReadStream stream buffer 0 256)))
			(display (string-append len " " uri))(newline)
			(if (> len 0)
                 (set! out (cons (decode buffer len) out)))
			len))  
    (display (string-append "Uri " uri " : " (resp.GetContentLength resp)))(newline)
    (do ((len 1 (read-buffer)))
        ((= len 0) (string-concat (reverse out))))
  ))

; sequential
;(for-each get-content '("http://microsoft.com" "http://live.com" "http://wintellect.com" "http://chayden.net"))

; parallel
; These are parallel because, AT THE TOP LEVEL, the REPL continues after getting Suspend.
(get-content "http://microsoft.com")
(get-content "http://live.com")  
(get-content "http://wintellect.com")
(p (get-content "http://chayden.net"))
 

(p "loaded")
