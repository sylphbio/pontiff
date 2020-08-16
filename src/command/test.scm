(module command.test (test)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process)
(import chicken.pathname)

(import tabulae)
(import ix)

(import (prefix state state:))
(import (prefix command.build command:))
(import (prefix command.gather command:))
(import util)

; XXX TODO use verbose to stop test egg from hiding callchains etc
(define (test argv)
  (define verbose ((^.v (keyw :verbose)) argv))
  (define test-type ((^.v (keyw :type)) argv))

  ; gather deps for build and test artifacts
  (when ((^.v (keyw :gather)) argv)
        (command:gather (ix:build 'pontiff:gather:argv :all #t :verbose verbose)))

  (when (memq test-type '(unit all))
        (command:build-artifact (find* (lambda (a) (eq? ((^.v (keyw :name)) a) 'unit))
                                       ((^.v (keyw :tests)) (state:pfile)))
                                verbose)
        (printf "running unit tests...\n")
        (process-join (process-create (make-pathname `(,(state:working-path) ,(state:build-dir)) "unit")
                                      '()
                                      (state:env))))

  (when (memq test-type '(integration all))
        (command:build-artifact (find* (lambda (a) (eq? ((^.v (keyw :name)) a) 'integration))
                                       ((^.v (keyw :tests)) (state:pfile)))
                                verbose)
        (printf "running integration tests...\n")
        (process-join (process-create (make-pathname `(,(state:working-path) ,(state:build-dir)) "integration")
                                      '()
                                      (state:env)))))

)
