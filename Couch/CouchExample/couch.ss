(load "couch-interface.ss")

(p "----- Scheme Program -----")
(define db (CouchDb.New "http://localhost:5984" "test_db"))

(p "Databases ")
(for-each p (vector->list (GetDatabases db)))(newline)

(display "Contents of database ")(p (DbName db))
(for-each (lambda (doc) (p (Id doc))) (vector->list (GetAllDocuments db)))


;; additional tests not in the C# version
(define ndocs (CountDocuments db))
(define server "http://localhost:5984")
(define created (CreateDatabase (CouchDb.New server "new_db")))
(define deleted (DeleteDatabase (CouchDb.New server "new_db")))
(define doc (CreateDocument db "{\"name\" : \"user\"}"))
(define count (CountDocuments db))

