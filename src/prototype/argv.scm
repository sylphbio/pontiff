(module prototype.argv *

(import scheme)
(import chicken.base)
(import chicken.type)

; XXX TODO --vcs to set vcs type, incl none
(define new `(pontiff:new:argv :project-name symbol
                               :artifact-type (enum pontiff:executable pontiff:library)
                               :artifact-name symbol))

(define init `(pontiff:init:argv))

(define build `(pontiff:build:argv :artifacts (list (sum (sexp pontiff:executable) (sexp pontiff:library)))
                                   :dry-run boolean
                                   :static boolean
                                   :verbose boolean))

(define run `(pontiff:run:argv :artifact symbol))

(define test `(pontiff:test:argv :test-type (enum unit integration all)))

; XXX TODO --deps to also wipe out dependencies
(define clean `(pontiff:clean:argv))

)
