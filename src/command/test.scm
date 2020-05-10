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
(import util)

(define (test argv)
  (define test-type ((^.!! (keyw :type)) argv))

  (when (memq test-type '(unit all))
        (command:build-artifact (find* (lambda (a) (eq? ((^.!! (keyw :name)) a) 'unit))
                                       ((^.!! (keyw :tests)) (state:pfile))))
        (printf "running unit tests...\n")
        (process-join (process-create (make-pathname `(,(state:working-path) ,(state:build-dir)) "unit")
                                      '()
                                      (state:env))))

  (when (memq test-type '(integration all))
        (command:build-artifact (find* (lambda (a) (eq? ((^.!! (keyw :name)) a) 'integration))
                                       ((^.!! (keyw :tests)) (state:pfile))))
        (printf "running integration tests...\n")
        (process-join (process-create (make-pathname `(,(state:working-path) ,(state:build-dir)) "integration")
                                      '()
                                      (state:env)))))

)
