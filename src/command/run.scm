(module command.run (run)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process)
(import chicken.process-context)
(import chicken.pathname)

(import tabulae)
(import ix)

(import (prefix state state:))
(import util)

(define (run argv)
  (define exe (symbol->string ((^.v (keyw :artifact) (keyw :name)) argv)))
  (printf "running ~A...\n" exe)
  (process-join (process-create (make-pathname `(,(state:working-path) ,(state:build-dir)) exe)
                                (map ix:unwrap ((^.v (keyw :exec-args)) argv))
                                (state:env))))

)
