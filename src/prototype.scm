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
    ((pontiff:module:file)   p-mfile)
    ((pontiff:state)         p-state)
    ; command-line arguments
    ((pontiff:new:argv)   argv:new)
    ((pontiff:init:argv)  argv:init)
    ((pontiff:build:argv) argv:build)
    ((pontiff:run:argv)   argv:run)
    ((pontiff:test:argv)  argv:test)
    ((pontiff:clean:argv) argv:clean)
    (else #f))))

)
