;; define types
(define CouchDb "CouchLib.CouchDb, CouchLib, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null")
(define DocInfo "CouchLib.CouchDb+DocInfo, CouchLib, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null")

;; define .NET methods
(define CouchDb.New (method CouchDb "New" "string" "string"))
(define GetDatabases (method CouchDb "GetDatabases"))
(define CountDocuments (method CouchDb "CountDocuments"))
(define GetAllDocuments (method CouchDb "GetAllDocuments"))
(define CreateDatabase (method CouchDb "CreateDatabase"))
(define DeleteDatabase (method CouchDb "DeleteDatabase"))
(define CreateDocument (method CouchDb "CreateDocument" "string"))
(define GetDocument (method CouchDb "GetDocument" "string"))
(define DeleteDocument (method CouchDb "DeleteDocument" DocInfo))
(define Id (method DocInfo "get_Id"))
(define Revision (method DocInfo "get_Revision"))

(p "running")
(define server "http://localhost:5984")
(define database "test_db")
(define db (CouchDb.New server database))
(define databases (GetDatabases db))
(define ndocs (CountDocuments db))
(define created (CreateDatabase (CouchDb.New server "new_db")))
(define deleted (DeleteDatabase (CouchDb.New server "new_db")))
(define doc (CreateDocument db "{\"name\" : \"user\"}"))
(define count (CountDocuments db))
(define all (GetAllDocuments db))
(define doc0 (GetDocument db (Id (vector-ref all 0))))

(define do-test
  (lambda (n) 
    (do ((i 0 (+ i 1))) ((= i n)) 
      (GetDocument db (Id (vector-ref all 0))))))

; gives microseconds per iteration
(define test (lambda (n) 
  (/ (* 1000 (caadr (time (do-test n)))) n)))

(begin (display (test 1000))(newline))

(p "loaded")
