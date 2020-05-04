(module command (dispatch)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae)
(import tabulae.monad)
(import ix)

(import command.new)
(import command.gather)
(import command.build)
(import command.run)
(import command.test)
(import command.clean)

(define (dispatch argv)
  (define tag (ix:ident->tag ((^.! ident) argv)))
  (to-either
    (case tag
      ((pontiff:new:argv)    (new argv))
      ((pontiff:gather:argv) (gather argv))
      ((pontiff:build:argv)  (build argv))
      ((pontiff:run:argv)    (run argv))
      ((pontiff:test:argv)   (test argv))
      ((pontiff:clean:argv)  (clean argv))
      (else #f))
    `(1 . "pontiff error: command not implemented")))

)
