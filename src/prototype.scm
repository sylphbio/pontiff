(module prototype *

(import scheme)
(import chicken.base)
(import chicken.type)

(import tabulae)
(import tabulae.monad)

(import (prefix prototype.file file:))
(import (prefix prototype.argv argv:))
(import prototype.internal)

(define (prototype tag)
  (to-maybe (case tag
    ; pontiff file and subobjects
    ((pontiff)            file:file)
    ((pontiff:repository) file:repo)
    ((pontiff:dependency) file:dep)
    ((pontiff:executable) file:exe)
    ((pontiff:library)    file:lib)
    ; internal objects
    ((pontiff:module)        p-module)
    ((pontiff:module:import) p-import)
    ((pontiff:state)         p-state)
    ; command-line arguments
    ((pontiff:argv:init)  argv:init)
    ((pontiff:argv:build) argv:build)
    ((pontiff:argv:run)   argv:run)
    ((pontiff:argv:test)  argv:test)
    ((pontiff:argv:clean) argv:clean)
    (else #f))))

)
