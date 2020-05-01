(module prototype.argv *

(import scheme)
(import chicken.base)
(import chicken.type)

; XXX TODO --vcs to set vcs type, incl none
(define new `(pontiff:new:argv :project-name symbol
                               :artifact-type (enum pontiff:executable pontiff:library)
                               :artifact-name symbol))

(define init `(pontiff:init:argv))

; XXX TODO --rebuild to ignore the hash stuff when I impl it
(define build `(pontiff:build:argv :artifacts (list (sum (sexp pontiff:executable) (sexp pontiff:library)))
                                   :dry-run boolean
                                   :static boolean
                                   :verbose boolean))

(define run `(pontiff:run:argv :artifact (sexp pontiff:executable)
                               :exec-args (list string)))

(define test `(pontiff:test:argv :type (enum unit integration all)))

; XXX TODO --deps to also wipe out dependencies
(define clean `(pontiff:clean:argv))

)
