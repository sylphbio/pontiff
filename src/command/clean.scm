(module command.clean (clean)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.file.posix)
(import chicken.pathname)
(import chicken.process-context)

(import tabulae)
(import ix)

(import (prefix state state:))

(define (simple-clean)
  (change-directory (state:build-dir))
  (for-each delete-file
            (filter* (lambda (file) (and (not (directory? file))
                                         (not (equal? file "deps.ix"))))
                     (directory)))
  (change-directory (state:working-path)))

; XXX should depclean also subinvoke clean on directory deps? something to consider
(define (dep-clean)
  (delete-directory (state:build-dir) #t)
  (create-directory (state:build-dir)))

(define (clean argv)
  (if ((^.!! (keyw :depclean)) argv)
      (dep-clean)
      (simple-clean))
  (printf "clean complete\n"))

)
