(module external-include ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.foreign)

#>
#include "two.h"
<#

(define ctwo (foreign-lambda int ctwo))

(include "two.scm")

(define (main)
  (if (= (+ (ctwo) (scmtwo)) 4)
      (exit 0)
      (exit 1)))

(main)

)
