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

(define build-grammar
  `((all      "build all"
              (single-char #\a))
    (artifact "build specified artifact"
              (single-char #\A)
              (value (required <name>)))
    (dry-run  "exit before invoking the compiler")
    (static   "link executable statically"
              (single-char #\s))
    (verbose  "verbose output"
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
    (unit        "run unit tests"
                 (single-char #\u))))

(define (getopt grammar argv)
  (do/m <maybe>
    (call-with-values (lambda () (getopt-long (cdr argv) grammar :unknown-option-handler (lambda (_) #f)))
                      (lambda (args success) (if success (return args) (fail))))))

(define (new-builder argv)
  (do/m <either>
    (args <- (maybe->either (getopt new-grammar argv)
                            `(1 . ,usage-string)))
    ; must have project name
    (project-name <- (if (= (length (alist-ref '@ args)) 1)
                         (return (string->symbol (car (alist-ref '@ args))))
                         (fail `(1 . "pontiff error: must specify project name"))))
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

(define (init-builder argv) '())

(define (build-builder argv)
  (do/m <either>
    (args <- (maybe->either (getopt build-grammar argv)
                            `(1 . ,usage-string)))
    ; no extraneous input
    (to-either (= (length (alist-ref '@ args)) 0)
               `(1 . "pontiff error: extraneous input"))
    ; all/artifact mutually exclusive
    (to-either (not (and (alist-ref 'all args) (alist-ref 'artifact args)))
               `(1 . "pontiff error: cannot specify --all and --artifact"))
    ; this is safe because I validate the file when state loads it
    (declare project-artifacts ((^.!! (keyw :artifacts)) (state:file)))
    ; as in, artifacts under consideration
    (artifacts <- (cond ((alist-ref 'artifact args)
                         (let* ((name (string->symbol (alist-ref 'artifact args)))
                                (a (find* (lambda (a) (eq? ((^.!! (keyw :name)) a) name)) project-artifacts)))
                               (if a
                                   (return `(,a))
                                   (fail `(1 . ,(<> "pontiff error: no artifact named " (symbol->string name)))))))
                        ((or (alist-ref 'all args) (= (length project-artifacts) 1))
                         (return project-artifacts))
                        (else (fail `(1 . "pontiff error: could not determine suitable artifact")))))
    ; static flag is only meaningful for executables because libraries always build both
    (to-either (not (and (alist-ref 'static args) (any* library? artifacts)))
               `(1 . "pontiff error: cannot specify --static for library"))
    (maybe->either (ix:build 'pontiff:build:argv :artifacts artifacts
                                                 :dry-run (alist-ref 'dry-run args)
                                                 :static (alist-ref 'static args)
                                                 :verbose (alist-ref 'verbose args))
                   `(1 . "pontiff error: failed to build argv ix"))))

(define (run-builder argv) '())

(define (test-builder argv) '())

(define (clean-builder argv) '())

(define (command-builder cmd)
  (to-maybe (case cmd
    ((new)   new-builder)
    ((init)  init-builder)
    ((build) build-builder)
    ((run)   run-builder)
    ((test)  test-builder)
    ((clean) clean-builder)
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

  init

  build
#{(lpad (usage build-grammar))}
  run
#{(lpad (usage run-grammar))}
  test
#{(lpad (usage test-grammar))}
    default behavior is to only run unit tests

  clean

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
