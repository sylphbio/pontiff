(module command.new.template *

(import scheme)
(import chicken.base)

(define executable #<<EOF
(module ##ROOT## ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(define (main)
  (display "hello pontiff!\n"))

(main)

)

EOF
)

(define library #<<EOF
(module ##ROOT## *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

)

EOF
)

(define test #<<EOF
(module ##ROOT## ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import test)

(import ##MODULENAME##)

(test-group "basic tests"
  (test "lgtm!" 4 (+ 2 2)))

(test-exit)

)

EOF
)

(define gitignore #<<EOF
*~
*.swp
*.swo
.pontiff-work/

EOF
)

(define (tag->template t)
  (case t
    ((pontiff:executable) executable)
    ((pontiff:library) library)
    (else (error "unknown template" t))))

)
