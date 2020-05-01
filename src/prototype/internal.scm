(module prototype.internal *

(import scheme)
(import chicken.base)
(import chicken.type)

(define p-module `(pontiff:module :name symbol
                                  :path string
                                  :file-hash string
                                  :subgraph-hash string
                                  :is-root boolean
                                  :local-imports (list symbol)))

(define p-mfile `(pontiff:module:file :dynamic (list (sexp pontiff:module))
                                      :static (list (sexp pontiff:module))))

(define p-state `(pontiff:state :working-path string
                                :build-dir string
                                :in-project boolean
                                :pfile (optional (sexp pontiff))
                                :mfile (sexp pontiff:module:file)))

)
