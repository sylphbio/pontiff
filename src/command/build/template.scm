(module command.build.template *

(import scheme)
(import chicken.base)

(define prologue #<<EOM
(import chicken.platform)
(repository-path `##REPOS##)

EOM
)

)
