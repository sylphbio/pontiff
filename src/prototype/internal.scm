(module prototype.internal *

(import scheme)
(import chicken.base)
(import chicken.type)

(define p-module `(pontiff:module :name symbol
                                  :path string
                                  :file-hash string
                                  :subgraph-hash string
                                  :is-root boolean
                                  :skip-compile boolean
                                  :local-imports (list symbol)))

(define mfile-block `(pontiff:module:block :name symbol
                                           :subgraph-hash string
                                           :is-root boolean
                                           :static boolean
                                           :dependent boolean))

(define p-state `(pontiff:state :working-path string
                                :build-dir string
                                :link-path string
                                :in-project boolean
                                :subinvocation boolean
                                :pfile (optional (sexp pontiff))
                                :mfile (sexp ix:*)
                                :env (list (product string string))))

)
