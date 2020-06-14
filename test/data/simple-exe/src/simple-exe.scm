(module simple-exe ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import (prefix srfi-1 srfi1:))
(import (prefix simple-lib lib:))

(define (main)
  (define one (car (srfi1:iota 1 1)))
  (define two (lib:two))

  (printf "~S\n" (+ one two))

  (exit 0))

(main)

)
