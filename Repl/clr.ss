;; clr.ss
;; clr tests

(define List "System.Collections.ArrayList")
(define ArrayList.Add (method List "Add" "System.Object"))
(define ArrayList.At (method List "get_Item" "System.Int32"))
(define lst (new List))
(ArrayList.Add lst 10)
(ArrayList.Add lst 20)
(ArrayList.Add lst 30)
; (p (ArrayList.At lst 1))

(define do-test
  (lambda (n lst) 
    (do ((i 0 (+ i 1))) ((= i n)) 
      (ArrayList.At lst 1))))

; gives microseconds per iteration
(define test (lambda (n lst) 
  (/ (* 1000 (caadr (time (do-test n lst)))) n)))

(begin (display (test 1000 lst))(newline))

