(module external-link ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import lmdb-ht)

(define (main)
  (define db (db-open "test"))
  (db-close db)
  (db-delete-database "test")
  (exit 0))

(main)

)
