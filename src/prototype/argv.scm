(module prototype.argv *

(import scheme)
(import chicken.base)
(import chicken.type)

(define init `(pontiff:argv:init :project-name symbol
                                 :artifact-type (enum pontiff:executable pontiff:library)
                                 :artifact-name symbol))

(define build `(pontiff:argv:build :artifacts (list (sum (sexp pontiff:executable) (sexp pontiff:library)))
                                   :dry-run boolean
                                   :static boolean
                                   :verbose boolean))

(define run `(pontiff:argv:run :artifact symbol))

(define test `(pontiff:argv:test :test-type (enum unit integration all)))

(define clean `(pontiff:argv:clean))

)
