(module prototype.file *

(import scheme)
(import chicken.base)
(import chicken.type)

(define file `(pontiff :name symbol
                       :version (product natural natural natural)
                       :pontiff-version (product natural natural natural)
                       :synopsis string
                       :authors (list string)
                       :maintainers (optional (list string))
                       :license string
                       :repository (sexp pontiff:repository)
                       :source-dir string
                       :test-dir string
                       :csc-flags (list string)
                       :cc (enum clang) ; XXX support gcc maybe
                       :ld (enum lld)   ; XXX support gnu ld maybe
                       :artifacts (list (sum (sexp pontiff:executable) (sexp pontiff:library)))
                       :tests (list (sexp pontiff:executable))
                       :dependencies (list (sexp pontiff:dependency))
                       :egg-resolver (enum chicken-install) ; XXX write my own resolver
                       :egg-dependencies (list symbol)))

(define repo `(pontiff:repository :vcs (enum git) ; XXX support svn, maybe mercurial, maybe pijul
                                  :url string))

; XXX eventually a way to specify version constraints
(define dep `(pontiff:dependency :name symbol
                                 :repo (sexp pontiff:repository))) ; XXX write a service to resolve these

(define exe `(pontiff:executable :name symbol
                                 :root symbol))

(define lib `(pontiff:library :name symbol
                              :root symbol))

)
