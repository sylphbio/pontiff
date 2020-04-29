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
  (define exe (symbol->string ((^.!! (keyw :artifact) (keyw :name)) argv)))
  (change-directory (state:build-dir))
  (printf "running ~A...\n" exe)
  (process-join (process-run (make-pathname "." exe) ((^.!! (keyw :exec-args)) argv)))
  (change-directory (state:working-path)))

)
