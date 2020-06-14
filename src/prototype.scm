(module prototype *

(import scheme)
(import chicken.base)
(import chicken.type)

(import tabulae)

(import (prefix prototype.file file:))
(import (prefix prototype.argv argv:))
(import prototype.internal)

(define prototypes (list
  ; pontiff.ix toplevel and subobjects
  file:file
  file:repo
  file:dir
  file:exe
  file:lib
  ; internal objects
  p-module
  p-state
  mfile-block
  dfile
  ; command-line arguments
  argv:new
  argv:gather
  argv:build
  argv:run
  argv:test
  argv:clean))

)
