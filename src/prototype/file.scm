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
                       ; XXX TODO separate out test deps
                       :dependencies (list (sum symbol (sexp pontiff:repository)))
                       :resolver (enum alice) ; highly advanced technology
                       :egg-dependencies (list symbol)
                       :egg-resolver (enum chicken-install))) ; XXX write my own resolver

(define repo `(pontiff:repository :name symbol
                                  :vcs (enum git) ; XXX support svn, maybe mercurial, maybe pijul
                                  :url string))

(define exe `(pontiff:executable :name symbol
                                 :root symbol))

(define lib `(pontiff:library :name symbol
                              :root symbol))

)
