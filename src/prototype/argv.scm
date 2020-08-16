(module prototype.argv *

(import scheme)
(import chicken.base)
(import chicken.type)

; XXX TODO --vcs to set vcs type, incl none
(define new `(pontiff:new:argv :project-name symbol
                               :artifact-type (enum pontiff:executable pontiff:library)
                               :artifact-name symbol))

; XXX TODO --force to ignore deps.ix when I impl that. should also clear modules.ix 
; this depends on the libgit switch, we want to clone *or pull*
(define gather `(pontiff:gather:argv :all boolean
                                     :verbose boolean))

(define build `(pontiff:build:argv :artifacts (list (sum (sexp pontiff:executable) (sexp pontiff:library)))
                                   :dry-run boolean
                                   :force  boolean
                                   :gather boolean
                                   :static boolean
                                   :verbose boolean))

(define run `(pontiff:run:argv :artifact (sexp pontiff:executable)
                               :exec-args (list string)))

; XXX TODO get rid of unit/integration hardcoding and make them arbitrary names
(define test `(pontiff:test:argv :type (enum unit integration all)
                                 :gather boolean
                                 :verbose boolean))

(define clean `(pontiff:clean:argv :extent (enum project deps all)))

)
