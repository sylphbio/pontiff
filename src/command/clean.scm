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

(define (targeted-clean deps)
  (change-directory (state:build-dir))

  (for-each delete-file
            (filter* (lambda (file) (and (not (directory? file))
                                         (not (equal? file "deps.ix"))))
                     (directory)))

  (when deps (delete-file* "deps.ix")
             (when (directory-exists? "deps")
                   (delete-directory "deps" #t)))

  (change-directory (state:working-path)))

(define (wanton-clean)
  (delete-directory (state:build-dir) #t)
  (create-directory (state:build-dir)))

(define (clean argv)
  (case ((^.v (keyw :extent)) argv)
    ((project) (targeted-clean #f))
    ((deps) (targeted-clean #t))
    ((all) (wanton-clean)))
  (printf "clean complete\n"))

)
