(module command.build.template *

(import scheme)
(import chicken.base)

(define prologue #<<EOF
(module pontiff-prologue ()

(import scheme)
(import chicken.base)

(cond-expand
  ((or pontiff-dynamic-executable pontiff-dynamic-library)
   (import chicken.process-context)
   (set-environment-variable! "CHICKEN_REPOSITORY_PATH" ##REPO##))
  (else))

)

EOF
)

)
