(module test.integration ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.process)
(import chicken.process-context)
(import chicken.pathname)

(import test)

; calling pontiff inside pontiff is always fun
(unset-environment-variable! "PONTIFF_LINK_PATH")
(unset-environment-variable! "CHICKEN_EGG_CACHE")
(unset-environment-variable! "CHICKEN_INSTALL_REPOSITORY")
(unset-environment-variable! "CHICKEN_INSTALL_PREFIX")
(unset-environment-variable! "CHICKEN_REPOSITORY_PATH")

(define pwd (current-directory))
(define tmp (create-temporary-directory))
(system* (string-append "cp -r test/data " tmp "/"))
(define dat (make-pathname `(,tmp "data") #f))

(define (pexe cmd . args)
  (define bin (make-pathname `(,pwd ".pontiff-work") "pontiff"))
  (system* (string-intersperse `(,bin ,cmd ,@args ">" "/dev/null"))))

(define (trivial-builds artifact #!optional exe)
  (pexe "clean")

  (test-assert "simple build" (pexe "build"))
  (when exe (test-assert "run" (pexe "run")))
  (pexe "clean")

  (test-assert "artifact build" (pexe "build" "-A" artifact))
  (when exe (test-assert "run" (pexe "run" "-A" artifact)))
  (pexe "clean")

  (test-assert "all build" (pexe "build" "-a"))
  (when exe (test-assert "run" (pexe "run")))

  (test-assert "force build" (pexe "build" "-f"))
  (when exe (test-assert "run" (pexe "run")))
  (pexe "clean")

  (test-assert "dryrun" (pexe "build" "--dry-run"))
  (pexe "clean")

  (when exe (test-assert "static build" (pexe "build" "--static"))
            (test-assert "run" (pexe "run"))
            (pexe "clean")))

(test-group "simple lib"
  (change-directory (make-pathname dat "simple-lib"))
  (trivial-builds "simple-lib")
  (change-directory tmp))

(test-group "simple exe"
  (change-directory (make-pathname dat "simple-exe"))
  (system* (string-append "sed -i 's|##TMPDIR##|" dat "|g' pontiff.ix"))
  (trivial-builds "simple-exe" #t)
  (change-directory tmp))

; XXX fetching/building eggs takes absolutely forever, I should probably mock that shit out
(test-group "bootstrap"
  (change-directory tmp)
  (create-directory "pontiff")
  (change-directory "pontiff")
  (system* (string-intersperse `("cp" ,(make-pathname pwd "bootstrap" "scm") ".")))
  (system* (string-intersperse `("cp" ,(make-pathname pwd "pontiff" "ix") ".")))
  (system* (string-intersperse `("cp" "-r" ,(make-pathname pwd "src") ".")))
  (test-assert "bootstrap.scm works" (system* "./bootstrap.scm > /dev/null"))
  (test-assert "resulting binary works" (system* ".pontiff-work/pontiff --help > /dev/null"))
  (change-directory tmp))

(test-exit)

)
