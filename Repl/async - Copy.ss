;; test async clr methods

;; define assembly names
(define WebRequest "System.Net.WebRequest, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define WebResponse "System.Net.WebResponse, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define Stream "System.IO.Stream")
(define Encoding "System.Text.Encoding")
(define Decoder "System.Text.Decoder")

;; define CLR methods
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
     (out-string ""))
     ; function to decode byte array into string
     ; return a string
	 (define (decode buffer len)
	   (let ((char-buffer (make-string 1000)))
		 (decoder.GetChars stream.Decode buffer 0 len char-buffer 0)
		 (substring char-buffer 0 len)
	 ))
     ; read a buffer of bytes from the stream and store into out-string
     ; return the number of bytes read
	 (define (read-buffer)
		(let* 
          ((buffer (new-array "byte" 256)) 
		   (len (stream.ReadStream stream buffer 0 256)))
			(p (string-append len " " uri))
			(if (> len 0)
                 (set! out-string (string-append out-string (decode buffer len))))
			len))  
    (p (string-append "Uri " uri " : " (resp.GetContentLength resp)))
    (do ((len 1 (read-buffer)))
        ((<= len 0) out-string))
    (p out-string)
  ))

;(do ((x 0 (+ 1 x))) ((> x 10) x) (p x))
;(do ((x 0)) ((> x 10) x) (set! x (+ 1 x)) (p x))

; sequential
;(for-each get-content '("http://microsoft.com" "http://live.com" "http://wintellect.com"))

; parallel
(get-content "http://microsoft.com")
(get-content "http://live.com")  
(get-content "http://wintellect.com")
(get-content "http://chayden.net")
 

(p "loaded")
