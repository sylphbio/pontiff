(module util *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae)
(import ix)

(define (executable? a) (and (ix:sexp? a)
                             (eq? (ix:ident->tag ((^.! ident) a)) 'pontiff:executable)))

(define (library? a) (and (ix:sexp? a)
                          (eq? (ix:ident->tag ((^.! ident) a)) 'pontiff:library)))

)
