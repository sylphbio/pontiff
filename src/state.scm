(module state (init version version-string working-path build-dir in-project pfile mfile save-pfile save-mfile)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process-context)
(import chicken.pathname)
(import chicken.file)
(import chicken.io)

(import tabulae)
(import tabulae.monad)
(import ix)

(import (prefix prototype prototype:))
(import util)

(define pstate #f)

; XXX TODO FIXME come up with some way of simply embedding the version from pontiff.ix into the binary
; so I don't have to worry about updating it in two places
; XXX UPDATE I could just like, put something like (define $VERSION asdf) up top with -prelude
; XXX wait why can't I just have a few standard things like __VERSION__ and __LINE__ that I just do text substitution
(define version '(0 1 0))
(define version-string (foldl (lambda (acc n) (<> acc "." (number->string n)))
                              (number->string (car version))
                              (cdr version)))

(define bdirname ".pontiff-work")
(define pfilename "pontiff.ix")
(define mfilename "modules.ix")

(define (init)
  ; set up ix prototypes
  (ix:init prototype:prototype)

  ; load pfile, if it exists
  (define pfile (do/m <maybe>
    (>>= (to-maybe (load-file pfilename))
         parse:ix
         ((curry* ix:validate-as) 'pontiff))))

  ; existence of a pfile means we are necessarily in a project
  (define in-project (just? pfile))
  (define pfile-kv (if in-project `(:pfile ,(from-just pfile)) '()))

  ; this is created by pontiff new but just in case the user got overzealous
  (when (and in-project (not (directory-exists? bdirname)))
        (create-directory bdirname))

  ; this is an object with lists of modules last built, if empty nbd
  (define mfile (do/m <maybe>
    (>>= (to-maybe (load-file (make-pathname bdirname mfilename)))
         parse:ix
         ((curry* ix:validate-as) 'pontiff:module:file))))

  (define mfile-v (if (just? mfile)
                      (from-just mfile)
                      (ix:build! 'pontiff:module:file :dynamic '() :static '())))

  (set! pstate (apply ix:build!
    `(pontiff:state :working-path ,(current-directory)
                    :build-dir ,bdirname
                    :in-project ,in-project
                    ,@pfile-kv
                    :mfile ,mfile-v))))

(define (access kw) (and pstate (ix:unwrap! ((^.! (keyw kw)) pstate))))

(define (working-path)   (access :working-path))
(define (build-dir)      (access :build-dir))
(define (in-project)     (access :in-project))
; file is optional, so this may fail. we force from-just for interface uniformity
; caller should check in-project first, that is the point of its existence
(define (pfile)          (and pstate ((^.! (keyw :pfile)) pstate)))
(define (mfile)          (and pstate ((^.! (keyw :mfile)) pstate)))

(define (save-pfile sx)
  (if (not (file-exists? pfilename))
      (save-file pfilename (pp-ix sx))
      (error "not allowed to clobber existing pfile, this should only be called in new")))

(define (save-mfile sx)
  (if (in-project)
      (save-file (make-pathname `(,(working-path) ,(build-dir)) mfilename)
                 (pp-ix sx))
      (error "cannot save mfile outside of project")))

)
