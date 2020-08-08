#!/bin/sh
#|
exec csi -s "$0" "$@"
|#

(import chicken.string)
(import chicken.format)
(import chicken.process)
(import chicken.file)
(import chicken.pathname)
(import chicken.process)
(import chicken.process-context)
(import chicken.io)

; I refuse to do real cli parsing but this should be sufficient
(define argv (command-line-arguments))
(when (and (not (null? argv)) (member (car argv) `("help" "-help" "--help" "-h")))
      (printf "usage: bootstrap.scm [<args>]\n  -n  don't pass -s to chicken-install\n")
      (exit 0))

; lol
(define chargv (flatten (map string->list argv)))
(define ci-sudo (if (member #\n chargv) '() `("-s")))

; get by printing in build-artifact: (map (lambda (m) (car (module->adjlist m))) sorted-modules)
(define tabulae-order `(tabulae.base tabulae.monad tabulae.parsec tabulae))
(define ix-order `(ix.static ix.base ix.stringify ix.lens ix.build ix.parse ix))
(define pontiff-order `(util prototype.internal prototype.argv prototype.file command.new.template command.build.template
                        graph prototype state argv command.new command.gather command.build.compiler command.run
                        command.clean command.build command.test command bin.pontiff))

; these just remember to keep current lol
(define egg-deps `("srfi-1" "srfi-34" "getopt-long" "simple-sha1" "uuid" "json" "test"))

; this is rude but I don't have my list helpers from tabulae so whatever
; hardly even in the top ten of things I'd have to fix if we ever support windows
(define (module->cscfile tag) (string-append (string-translate (symbol->string tag) "." "/") ".scm"))

; simple shorthand
(define (exec bin argv) (process-wait (process-run "/usr/bin/env" (cons bin argv))))

; copy-pasted from util.scm
(define (save-file path str)
  (call-with-output-file path (lambda (p) (write-string str #f p))))

; same
(define (load-file path)
  (and (file-exists? path)
       (file-readable? path)
       (let ((s (call-with-input-file path (lambda (p) (read-string #f p)))))
            (if (eof-object? s) "" s))))

; set up locations
(define pwd (current-directory))
(define tmp (create-temporary-directory))
(change-directory tmp)

; install system eggs
(printf "installing eggs (system)\n")
(exec "chicken-install" `(,@ci-sudo ,@egg-deps))

; clone ix and tabulae
(printf "cloning dependencies (temp)\n")
(exec "git" `("clone" "https://github.com/sylphbio/tabulae.git" "tabulae"))
(exec "git" `("clone" "https://github.com/sylphbio/ix.git" "ix"))

; this takes fully 30 seconds to compile on my laptop (from 2019!) lol
; note I did in fact confirm this only works because I sort the modules
(printf "building...\n")
(define cscfiles (append (map (lambda (tag) (make-pathname `(,tmp "tabulae" "src") (module->cscfile tag))) tabulae-order)
                         (map (lambda (tag) (make-pathname `(,tmp "ix" "src") (module->cscfile tag))) ix-order)
                         (map (lambda (tag) (make-pathname `(,pwd "src") (module->cscfile tag))) pontiff-order)))

(save-file "pontiff.scm" (string-intersperse (map load-file cscfiles) "\n"))
(exec "csc" `("-o" "pontiff" "-keyword-style" "prefix" "pontiff.scm"))

; it'd be nice to not have to get eggs/deps a second time but I'd rather keep this script simple
(printf "done! passing control to pontiff\n")
(change-directory pwd)
(process-execute (make-pathname tmp "pontiff") `("build" "--static"))
