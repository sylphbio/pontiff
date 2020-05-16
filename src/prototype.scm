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
    ((pontiff:directory)  file:dir)
    ((pontiff:executable) file:exe)
    ((pontiff:library)    file:lib)
    ; internal objects
    ((pontiff:module)        p-module)
    ((pontiff:state)         p-state)
    ((pontiff:module:block)  mfile-block)
    ((pontiff:state:deplist) dfile)
    ; command-line arguments
    ((pontiff:new:argv)    argv:new)
    ((pontiff:gather:argv) argv:gather)
    ((pontiff:build:argv)  argv:build)
    ((pontiff:run:argv)    argv:run)
    ((pontiff:test:argv)   argv:test)
    ((pontiff:clean:argv)  argv:clean)
    (else #f))))

)
