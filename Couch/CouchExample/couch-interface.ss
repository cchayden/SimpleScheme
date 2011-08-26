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
(define DbName (method CouchDb "get_DbName"))
(define Id (method DocInfo "get_Id"))
(define Revision (method DocInfo "get_Revision"))
