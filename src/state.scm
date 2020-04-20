(module state (init version version-string pfilename working-path build-dir in-project file)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process-context)
(import chicken.file)
(import chicken.io)

(import tabulae)
(import tabulae.monad)
(import ix)

(import (prefix prototype prototype:))

; XXX TODO FIXME come up with some way of simply embedding the version from pontiff.ix into the binary
; so I don't have to worry about updating it in two places
; XXX UPDATE I could just like, put something like (define $VERSION asdf) up top with -prelude
; XXX wait why can't I just have a few standard things like __VERSION__ and __LINE__ that I just do text substitution
(define version '(0 1 0))
(define version-string (foldl (lambda (acc n) (<> acc "." (number->string n))) (number->string (car version)) (cdr version)))

(define (pfilename) "pontiff.ix")
(define pstate '())

(define (init)
  (ix:init prototype:prototype)
  (define pfile (do/m <maybe>
    (pfile-str <- (to-maybe (and (file-exists? (pfilename))
                                 (file-readable? (pfilename))
                                 (call-with-input-file (pfilename) (lambda (p) (read-string #f p))))))
    (pfile-ix <- (parse:ix pfile-str))
    (ix:validate-as 'pontiff pfile-ix)))
  (set! pstate (apply ix:build! `(pontiff:state :working-path ,(current-directory)
                                                :build-dir ".pontiff-work"
                                                :in-project ,(just? pfile)
                                                ,@(if (just? pfile)
                                                      `(:file ,(from-just pfile))
                                                      '())))))

(define (access kw) (and pstate (ix:unwrap! ((^.! (keyw kw)) pstate))))

(define (working-path)   (access :working-path))
(define (build-dir)      (access :build-dir))
(define (in-project)     (access :in-project))
; file is optional, so this may fail. we force from-just for interface uniformity
; caller should check in-project first, that is the point of its existence
(define (file)           (and pstate ((^.! (keyw :file)) pstate)))

)
