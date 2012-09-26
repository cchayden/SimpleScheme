;; test async clr methods

;; define .NET type names
(define WebRequest "System.Net.WebRequest, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define WebResponse "System.Net.WebResponse, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")
(define Stream "System.IO.Stream")
(define Encoding "System.Text.Encoding")
(define Decoder "System.Text.Decoder")

;; define .NET methods
(define WebRequest.Create (method WebRequest "Create" "string"))
(define req.GetResponse (method-async WebRequest "GetResponse"))
(define stream.ReadStream (method-async Stream "Read" "byte[]" "int" "int"))
(define resp.ContentLength (method WebResponse "get_ContentLength"))
(define resp.GetResponseStream (method WebResponse "GetResponseStream"))
(define encoding.UTF8 (method Encoding "get_UTF8"))
(define encoding.GetDecoder (method Encoding "GetDecoder"))
(define stream.Decode (encoding.GetDecoder (encoding.UTF8)))
(define decoder.GetChars (method Decoder "GetChars" "byte[]" "int" "int" "char[]" "int"))


; get content asynchronously
(define (get-content uri)
  (let*
    ((resp (req.GetResponse (WebRequest.Create uri)))
     (stream (resp.GetResponseStream resp))
     (out '()))
     ; function to decode byte array into string
     ; return a string
	 (define (decode buffer len)
	   (let ((char-buffer (make-string 256)))
		 (decoder.GetChars stream.Decode buffer 0 len char-buffer 0)
		 (substring char-buffer 0 len)
	 ))
     ; read a buffer of bytes from the stream and append decoded buffer to out
     ; return the number of bytes read
	 (define (read-buffer)
		(let* 
          ((buffer (new-array "byte" 256)) 
		   (len (stream.ReadStream stream buffer 0 256)))
		  (display (string-append (number->string len)))(newline)
		  (if (> len 0)
              (set! out (cons (decode buffer len) out)))
		  len))  
    (display (string-append uri " " (number->string (resp.ContentLength resp))))(newline)
    (do ((len 1 (read-buffer)))
        ((= len 0) (string-concat (reverse out))))
  ))
