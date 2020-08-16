(module prototype.file (file repo dir exe lib)

(import scheme)
(import chicken.base)
(import chicken.type)

; mostly following egg categories, with the following changes:
; * lang-exts -> lang
; * debugging -> debug
; * xml -> (subsumed under parse)
; * doc-tools -> doc
; * egg-tools -> (subsumed under tools)
; * data -> cs
; * parsing -> parse
; * testing -> test
; * code-generation -> codegen
; * hell -> thread
; we also add the tags list, which allows multiple unconstrained appellations
(define cats `(lang graphics debug logic net io db os ffi web doc math oop cs parse tools
               sound test crypto ui codegen macros misc thread uncategorized obselete))

(define file `(pontiff :name symbol
                       :version (product natural natural natural)
                       :pontiff-version (product natural natural natural)
                       :synopsis string
                       :authors (list string)
                       :maintainers (optional (list string))
                       :license string
                       :repository (sexp pontiff:repository)
                       :website (optional string)
                       :category (enum ,@cats)
                       :tags (list symbol)
                       :source-dir string
                       :test-dir string
                       :csc-flags (list string)
                       :cc (enum clang) ; XXX support gcc maybe
                       :ld (enum lld)   ; XXX support gnu ld maybe
                       :artifacts (list (sum (sexp pontiff:executable) (sexp pontiff:library)))
                       :tests (list (sexp pontiff:executable))
                       :dependencies (list (sum symbol (sexp pontiff:repository) (sexp pontiff:directory)))
                       :test-dependencies (optional (list (sum symbol (sexp pontiff:repository) (sexp pontiff:directory))))
                       :resolver (enum alice) ; highly advanced technology
                       :lib-dependencies (list symbol)
                       :egg-dependencies (list symbol)
                       :egg-test-dependencies (optional (list symbol))
                       :egg-resolver (enum chicken-install))) ; XXX write my own resolver

(define repo `(pontiff:repository :name symbol
                                  :vcs (enum git) ; XXX support svn, maybe mercurial, maybe pijul
                                  ; XXX support branch
                                  :url string))

(define dir `(pontiff:directory :name symbol
                                :path string))

(define exe `(pontiff:executable :name symbol
                                 :root symbol))

(define lib `(pontiff:library :name symbol
                              :root symbol))

)
