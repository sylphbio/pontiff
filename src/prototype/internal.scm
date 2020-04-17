(module prototype.internal *

(import scheme)
(import chicken.base)
(import chicken.type)

(define p-module `(pontiff:module :name symbol
                                  :path string
                                  :is-root boolean
                                  :imports (list pontiff:module:import)))

(define p-import `(pontiff:module:import :name symbol
                                         :type (local dependency system)))

(define p-state `(pontiff:state :working-path string
                                :build-dir string
                                :in-project boolean
                                :file (optional (sexp pontiff))))

)
