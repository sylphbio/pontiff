(module argv *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae)
(import tabulae.monad)
(import ix)
(import getopt-long)

(import (prefix state state:))
(import util)

(define new-grammar
  `((executable "start project with executable artifact"
                (single-char #\e)
                (value (optional <name>)))
    (library    "start project with library artifact"
                (single-char #\l)
                (value (optional <name>)))))

(define gather-grammar
  `((verbose  "verbose output"
              (single-char #\v))))

(define build-grammar
  `((all       "build all artifacts"
               (single-char #\a))
    (all-libs  "build all libraries")
    (artifact  "build specified artifact"
               (single-char #\A)
               (value (required <name>)))
    (dry-run   "exit before invoking the compiler")
    (force     "build all compilation units even if unchanged"
               (single-char #\f))
    (no-gather "do not fetch or build dependencies")
    (static    "link executable statically"
               (single-char #\s))
    (verbose   "verbose output"
               (single-char #\v))))

(define run-grammar
  `((artifact "run specified artifact"
              (single-char #\A)
              (value (required <name>)))))

(define test-grammar
  `((all         "run all tests"
                 (single-char #\a))
    (integration "run integration tests"
                 (single-char #\i))
    (unit        "run unit tests (default)"
                 (single-char #\u))))

(define clean-grammar
  `((depclean "also remove all dependencies"
              (single-char #\d))))

(define (getopt grammar argv)
  (do/m <maybe>
    (call-with-values (lambda () (getopt-long (cdr argv) grammar :unknown-option-handler (lambda (_) #t)))
                      (lambda (args unknown) (if (eqv? unknown #t) (fail) (return args))))))

; helper function for build/run that searches the project's artifacts for a cli-specified one
(define (locate-artifact artifact-list tag)
  (do/m <either>
    (let ((a (find* (lambda (a) (eq? ((^.!! (keyw :name)) a) tag)) artifact-list)))
         (if a
             (return a)
             (fail `(1 . ,(<> "pontiff error: no artifact named " (symbol->string tag))))))))

(define (new-builder argv)
  (do/m <either>
    (args <- (maybe->either (getopt new-grammar argv)
                            `(1 . ,usage-string)))
    ; must have project name
    (project-name <- (cond ((= (length (alist-ref '@ args)) 1) (return (string->symbol (car (alist-ref '@ args)))))
                           ((= (length (alist-ref '@ args)) 0) (fail `(1 . "pontiff error: extraneous input")))
                           (else (fail `(1 . "pontiff error: must specify project name")))))
    (to-either (and (ix:well-typed? (ix:wrap 'symbol project-name))
                    (not (any* (lambda (c) (memq c `(#\: #\.))) (string->list (symbol->string project-name)))))
               `(1 . "pontiff error: project name should be valid ix symbol without periods or colons"))
    ; exe/lib are mutually exclusive
    (to-either (not (and (alist-ref 'executable args) (alist-ref 'library args)))
               `(1 . "pontiff error: cannot specify --executable and --library"))
    ; default to executable
    (declare artifact-type (if (alist-ref 'library args)
                               'library
                               'executable))
    ; default name artifact same as project
    (declare artifact-name (if (string? (alist-ref artifact-type args))
                               (string->symbol (alist-ref artifact-type args))
                               project-name))
    (maybe->either (ix:build 'pontiff:new:argv :project-name project-name
                                               :artifact-type (symbol-append 'pontiff: artifact-type)
                                               :artifact-name artifact-name)
                   `(1 . "pontiff error: failed to build argv ix"))))

(define (gather-builder argv)
  (do/m <either>
    (args <- (maybe->either (getopt build-grammar argv)
                            `(1 . ,usage-string)))
    ; no extraneous input
    (to-either (= (length (alist-ref '@ args)) 0)
               `(1 . "pontiff error: extraneous input"))
    (maybe->either (ix:build 'pontiff:gather:argv :verbose (alist-ref 'verbose args))
                   `(1 . "pontiff error: failed to build argv ix"))))

(define (build-builder argv)
  (do/m <either>
    (args <- (maybe->either (getopt build-grammar argv)
                            `(1 . ,usage-string)))
    ; no extraneous input
    (to-either (= (length (alist-ref '@ args)) 0)
               `(1 . "pontiff error: extraneous input"))
    ; this is safe because I validate the file when state loads it
    (declare project-artifacts ((^.!! (keyw :artifacts)) (state:pfile)))
    ; all > all-libs > artifact > implicit
    (build-artifacts <- (cond ((alist-ref 'all args) (return project-artifacts))
                              ((alist-ref 'all-libs args) (return (filter* library? project-artifacts)))
                              ((alist-ref 'artifact args)
                               (<$> list (locate-artifact project-artifacts (string->symbol (alist-ref 'artifact args)))))
                              ((= (length project-artifacts) 1) (return project-artifacts))
                              (else (fail `(1 . "pontiff error: could not determine suitable artifact")))))
    ; static flag is only meaningful for executables because libraries always build both
    (to-either (not (and (alist-ref 'static args) (any* library? build-artifacts)))
               `(1 . "pontiff error: cannot specify --static for libraries"))
    (maybe->either (ix:build 'pontiff:build:argv :artifacts build-artifacts
                                                 :dry-run (alist-ref 'dry-run args)
                                                 :force (alist-ref 'force args)
                                                 :gather (not (alist-ref 'no-gather args))
                                                 :static (alist-ref 'static args)
                                                 :verbose (alist-ref 'verbose args))
                   `(1 . "pontiff error: failed to build argv ix"))))

(define (run-builder argv)
  (do/m <either>
    ; take index of double dash excludes it, then we drop one extra to drop it
    (declare argv-div (or (findi* (lambda (a) (equal? a "--")) argv) (length argv)))
    (args <- (maybe->either (getopt run-grammar (take* argv-div argv))
                            `(1 . ,usage-string)))
    ; no extraneous input before double dash
    (to-either (= (length (alist-ref '@ args)) 0)
               `(1 . "pontiff error: extraneous input"))
    ; get pfile artifacts
    (declare project-artifacts ((^.!! (keyw :artifacts)) (state:pfile)))
    (artifact <- (cond ((alist-ref 'artifact args)
                        (locate-artifact project-artifacts (string->symbol (alist-ref 'artifact args))))
                       ((= (length (filter* executable? project-artifacts)) 1)
                        (return (find* executable? project-artifacts)))
                       (else (fail `(1 . "pontiff error: could not determine suitable artifact")))))
    (maybe->either (ix:build 'pontiff:run:argv :artifact artifact :exec-args (drop* (+ argv-div 1) argv))
                   `(1 . "pontiff error: failed to build argv ix"))))

; XXX TODO restructure this kinda like build with a pref stack of what actually to run
; --unit and --integration should be shorthands for an -A flag of those names
; and we pass throuhg a list of artifacts to foreach, rather than expecting our format is hardcoded
; because we allow it to be changed so we should be flexible for that eventuality
; all > unit = integration > artifact > implicit unit > implicit single
(define (test-builder argv)
  (do/m <either>
    (args <- (maybe->either (getopt test-grammar argv)
                            `(1 . ,usage-string)))
    ; no extraneous input
    (to-either (= (length (alist-ref '@ args)) 0)
               `(1 . "pontiff error: extraneous input"))
    (declare unit-flag (alist-ref 'unit args))
    (declare integ-flag (alist-ref 'integration args))
    (declare all-flag (alist-ref 'all args))
    (declare test-type (cond ((or all-flag (and unit-flag integ-flag)) 'all)
                             (integ-flag 'integration)
                             (else 'unit)))
    (maybe->either (ix:build 'pontiff:test:argv :type test-type)
                   `(1 . "pontiff error: failed to build argv ix"))))

(define (clean-builder argv)
  (do/m <either>
    (args <- (maybe->either (getopt clean-grammar argv)
                            `(1 . ,usage-string)))
    ; no extraneous input
    (to-either (= (length (alist-ref '@ args)) 0)
               `(1 . "pontiff error: extraneous input"))
    (maybe->either (ix:build 'pontiff:clean:argv :depclean (alist-ref 'depclean args))
                   `(1 . "pontiff error: failed to build argv ix"))))

(define (command-builder cmd)
  (to-maybe (case cmd
    ((new)    new-builder)
    ((gather) gather-builder)
    ((build)  build-builder)
    ((run)    run-builder)
    ((test)   test-builder)
    ((clean)  clean-builder)
    (else #f))))

(define usage-string
  (let ((lpad (lambda (s) (string-intersperse (map (lambda (l) (if (> (string-length l) 0) (<> "   " l) l))
                                                   (string-split s "\n" #t))
                                              "\n"))))
       #<#EOF
usage: pontiff <command> [<args>]

commands:
  new <project name>
#{(lpad (usage new-grammar))}
    by default new will start a project with one executable of the same name as the project

  gather
#{(lpad (usage gather-grammar))}
  build
#{(lpad (usage build-grammar))}
  run
#{(lpad (usage run-grammar))}
  test
#{(lpad (usage test-grammar))}
  clean
#{(lpad (usage clean-grammar))}
EOF

))

; left represents an early exit and is a pair of code and message
(define (process argv)
  (do/m <either>
    (cmd <- (to-either (and (not (null? argv)) (string->symbol (head* argv)))
                       `(1 . ,usage-string)))
    (to-either (not (memq cmd `(-h help --help -help)))
               `(0 . ,usage-string))
    (to-either (not (memq cmd `(-version --version)))
               `(0 . (<> "pontiff version: " state:version-string)))
    (to-either (or (eq? cmd 'new) (state:in-project))
               `(1 . "pontiff error: pontiff.ix missing, unreadable, or malformed"))
    (arg-builder <- (maybe->either (command-builder cmd)
                                   `(1 . ,(<> "pontiff error: invalid command " (symbol->string cmd)))))
    (arg-builder argv)))

)
