(module state (init version version-string working-path build-dir link-path in-project subinvocation
               pfile mfile dfile save-pfile save-mfile save-dfile env)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process)
(import chicken.process-context)
(import chicken.pathname)
(import chicken.port)
(import chicken.file)
(import chicken.file.posix)
(import chicken.foreign)
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

; should be /usr/lib/chicken/BINARYVERSION on normal systems
(define libdir (foreign-value "C_INSTALL_EGG_HOME" c-string))

; simple hardcoded names
(define bdirname ".pontiff-work")
(define pfilename "pontiff.ix")
(define mfilename "modules.ix")
(define dfilename "deps.ix")

; sets up subdirectories and symlinks system libs when not subinvoked
; everything this does is idempotent
(define (init-build-dir link-path)
  ; straightforward
  (create-directory (make-pathname bdirname "deps"))
  (create-directory (make-pathname bdirname "eggs"))
  (create-directory (make-pathname bdirname "sys"))

  ; extremely annoyingly chicken dumps its eggs and its system import libs all in the same directory
  ; we need to symlink system libs, otherwise chicken-install won't install egg dependencies
  ; this all goes away when pontiff manages the compiler and stdlib itself
  (for-each (lambda (src) (let ((dst (make-pathname `(,link-path "sys")
                                                    (pathname-strip-directory src))))
                               (when (not (file-exists? dst)) (create-symbolic-link src dst))))
            (glob (make-pathname libdir "chicken.*.import.so")
                  (make-pathname libdir "srfi-4.import.so")
                  (make-pathname libdir "types.db"))))

(define (init)
  ; set up ix prototypes
  (ix:init prototype:prototype)

  (define pwd (current-directory))
  (set-buffering-mode! (current-output-port) :full)

  ; load pfile, if it exists
  (define pfile (do/m <maybe>
    (>>= (to-maybe (load-file pfilename))
         parse:ix
         ((curry* ix:validate-as) 'pontiff))))

  ; existence of a pfile means we are necessarily in a project
  (define in-project (just? pfile))
  (define pfile-kv (if in-project `(:pfile ,(from-just pfile)) '()))

  ; we set this in gather when invoking build for dependencies
  (define subinv (let ((v (get-environment-variable "PONTIFF_SUBINVOCATION")))
                      (cond ((not v) #f)
                            ((equal? v "0") #f)
                            ((equal? v "1") #t)
                            (else (die "PONTIFF_SUBINVOCATION should be undef 0 or 1")))))

  ; set this to our own dir if not set, so subinvocations use our dir as a link target
  (define linkpath (or (get-environment-variable "PONTIFF_LINK_PATH")
                       (make-pathname pwd bdirname)))

  ; always need build dir, but subinvs use the caller's egg/sys
  (when in-project (create-directory bdirname))
  (when (and in-project (not subinv)) (init-build-dir linkpath))

  ; this is an ix:* where keys are module name dash static/dynamic, values are mfile block objects
  (define mfile (do/m <maybe>
    (>>= (to-maybe (load-file (make-pathname bdirname mfilename)))
         parse:ix)))

  (define mfile-v (if (just? mfile)
                      (from-just mfile)
                      (ix:build! 'ix:*)))

  ; this is two lists of symbols, corresponding to eggs and deps last built
  (define dfile (do/m <maybe>
    (>>= (to-maybe (load-file (make-pathname bdirname dfilename)))
         parse:ix
         ((curry* ix:validate-as) 'pontiff:state:deplist))))

  (define dfile-v (if (just? dfile)
                      (from-just dfile)
                      (ix:build! 'pontiff:state:deplist :eggs '() :deps '())))

  ; set these via process-create when using chicken-install or invoking chicken binaries
  ; converts to alist on access. I don't support scheme pairs in ix (vehemently so)
  (define deppath (make-pathname linkpath "deps"))
  (define eggpath (make-pathname linkpath "eggs"))
  (define syspath (make-pathname linkpath "sys"))
  (define env `(("PONTIFF_LINK_PATH" ,linkpath)
                ("CHICKEN_EGG_CACHE" ,eggpath)
                ("CHICKEN_INSTALL_REPOSITORY" ,eggpath)
                ("CHICKEN_REPOSITORY_PATH" ,(<> deppath ":" eggpath ":" syspath))))

  (set! pstate (apply ix:build!
    `(pontiff:state :working-path ,pwd
                    :build-dir ,bdirname
                    :link-path ,linkpath
                    :in-project ,in-project
                    :subinvocation ,subinv
                    ,@pfile-kv
                    :mfile ,mfile-v
                    :dfile ,dfile-v
                    :env ,env))))

(define (access kw) (and pstate ((^.! (keyw kw)) pstate)))
(define (access! kw) (and pstate ((^.!! (keyw kw)) pstate)))

(define (working-path)  (access! :working-path))
(define (build-dir)     (access! :build-dir))
(define (link-path)     (access! :link-path))
(define (in-project)    (access! :in-project))
(define (subinvocation) (access! :subinvocation))
; file is optional, so this may fail. we force from-just for interface uniformity
; we gate any command except new by in-project anyway
(define (pfile)         (access :pfile))
(define (mfile)         (access :mfile))
(define (dfile)         (access :dfile))
(define (env)           (map (lambda (prod) (cons (ix:unwrap! (second* prod)) (ix:unwrap! (third* prod)))) (access! :env)))

(define (save-pfile sx)
  (if (not (file-exists? pfilename))
      (save-file pfilename (pp-ix sx))
      (error "not allowed to clobber existing pfile, this should only be called in new")))

(define (save-mfile sx)
  (when (not (in-project)) (error "cannot save mfile outside of project"))
  (save-file (make-pathname `(,(working-path) ,(build-dir)) mfilename) (pp-ix sx))
  (set! pstate ((.~! sx (keyw :mfile)) pstate)))

(define (save-dfile sx)
  (when (not (in-project)) (error "cannot save dfile outside of project"))
  (save-file (make-pathname `(,(working-path) ,(build-dir)) dfilename) (pp-ix sx))
  (set! pstate ((.~! sx (keyw :dfile)) pstate)))

)
