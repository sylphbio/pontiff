(module state (init version version-string working-path build-dir link-path repo-path in-project subinvocation
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
(import chicken.read-syntax)

(import tabulae)
(import tabulae.monad)
(import ix)

(import prototype)
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

; symlinks a list of paths into dstdir
; we use this to set up inlcudes and syslibs for our eggy sandbox
(define (symlink-files link-path dstdir files)
  (for-each (lambda (src) (let ((dst (make-pathname `(,link-path ,dstdir)
                                                    (pathname-strip-directory src))))
                               (when (not (or (file-exists? dst) (symbolic-link? dst)))
                                     (create-symbolic-link src dst))))
            files))

; sets up subdirectories and symlinks system libs when not subinvoked
; everything this does is idempotent
(define (init-build-dir pwd link-path)
  ; straightforward
  (create-directory (make-pathname bdirname "deps"))
  (create-directory (make-pathname bdirname "eggs"))
  (create-directory (make-pathname bdirname "sys"))
  (create-directory (make-pathname bdirname "include"))
  (create-directory (make-pathname bdirname "share"))
  (create-directory (make-pathname bdirname "bin"))

  ; extremely annoyingly chicken dumps its eggs and its system import libs all in the same directory
  ; we need to symlink system libs, otherwise chicken-install won't install egg dependencies
  ; this all goes away when pontiff manages the compiler and stdlib itself
  (symlink-files link-path "sys" (glob (make-pathname libdir "chicken.*.import.so")
                                 (make-pathname libdir "srfi-4.import.so")
                                 (make-pathname libdir "types.db")))

  ; we also symlink in any includes the project might have
  ; as above, this will agglomerate everything in the ultimate root build dir
  ; XXX TODO document somewhere the fact that this feature exists lol. or create include dir for new projects
  ; XXX TODO FIXME good job genius now you need to keep hashes of these files too
  ; lazy way is just rebuild if any change
  ; glowy genius brain way is to treat them as if they were leaf modules and include them in subgraph hashes
  ; XXX TODO FIXME this should almost certainly happen for subinvs, but I need to test
  (when (directory-exists? "include")
        (symlink-files link-path "include" (glob (make-pathname `(,pwd "include") "*")))))

(define (init)
  ; set up ix prototypes
  (ix:register! prototypes)

  (define pwd (current-directory))
  (set-buffering-mode! (current-output-port) :full)

  ; load pfile, if it exists
  (define pfile (let ((pf (load-file pfilename)))
                     (and pf (ix:validate-as 'pontiff (parse:ix pf)))))

  ; existence of a pfile means we are necessarily in a project
  (define in-project (not (not pfile)))
  (define pfile-kv (if in-project `(:pfile ,pfile) '()))

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
  (when (and in-project (not subinv)) (init-build-dir pwd linkpath))

  ; this weirdo thing is just to transform inline c into something read won't choke on
  ; obviously this fails if you include the closing sharp in a c string or comment
  ; so... don't do that!
  (set-sharp-read-syntax! #\> (lambda (p) (letrec ((f (lambda (p acc)
    (let ((c (read-char p)))
         (cond ((and (eqv? c #\<) (eqv? (peek-char p) #\#))
                (read-char p)
                (list->string (reverse acc)))
               ((eof-object? c)
                (die "eof while parsing foreign sharp"))
               (else
                (f p (cons c acc))))))))
    `(foreign-declare ,(f p '())))))

  ; this is generic ix where keys are module name dash static/dynamic, values are mfile block objects
  (define mfile (let ((mf (load-file (make-pathname bdirname mfilename))))
                     (if mf (parse:ix mf) (ix:build 'ix))))

  ; this is two lists of symbols, corresponding to eggs and deps last built
  (define dfile (let ((df (load-file (make-pathname bdirname dfilename))))
                (if df
                    (ix:validate-as 'pontiff:state:deplist (parse:ix df))
                    (ix:build 'pontiff:state:deplist :eggs '() :deps '() :libs '()))))

  ; set these via process-create when using chicken-install or invoking chicken binaries
  ; converts to alist on access. I don't support scheme pairs in ix (vehemently so)
  ; XXX do I need to symlink anything in the share/include/bin dirs??
  ; install prefix was enough to fix srfi-26 but I dunno what all people do with this
  (define deppath (make-pathname linkpath "deps"))
  (define eggpath (make-pathname linkpath "eggs"))
  (define syspath (make-pathname linkpath "sys"))
  (define repopath (<> linkpath ":" deppath ":" eggpath ":" syspath))
  (define env `(("PONTIFF_LINK_PATH" ,linkpath)
                ("CHICKEN_EGG_CACHE" ,eggpath)
                ("CHICKEN_INSTALL_REPOSITORY" ,eggpath)
                ("CHICKEN_INSTALL_PREFIX" ,linkpath)
                ("CHICKEN_REPOSITORY_PATH" ,repopath)))

  (set! pstate (apply ix:build
    `(pontiff:state :working-path ,pwd
                    :build-dir ,bdirname
                    :link-path ,linkpath
                    :repo-path ,repopath
                    :in-project ,in-project
                    :subinvocation ,subinv
                    ,@pfile-kv
                    :mfile ,mfile
                    :dfile ,dfile
                    :env ,env))))

(define (access kw) (and pstate ((^. (keyw kw)) pstate)))
(define (access-u kw) (and pstate ((^.v (keyw kw)) pstate)))

(define (working-path)  (access-u :working-path))
(define (build-dir)     (access-u :build-dir))
(define (link-path)     (access-u :link-path))
(define (repo-path)     (access-u :repo-path))
(define (in-project)    (access-u :in-project))
(define (subinvocation) (access-u :subinvocation))
; file is optional, so this may fail. we force from-just for interface uniformity
; we gate any command except new by in-project anyway
(define (pfile)         (access :pfile))
(define (mfile)         (access :mfile))
(define (dfile)         (access :dfile))
(define (env)           (map (lambda (prod) (cons (ix:unwrap (second* prod)) (ix:unwrap (third* prod)))) (access-u :env)))

(define (save-pfile sx)
  (if (not (file-exists? pfilename))
      (save-file pfilename (pp-ix sx))
      (error "not allowed to clobber existing pfile, this should only be called in new")))

(define (save-mfile sx)
  (when (not (in-project)) (error "cannot save mfile outside of project"))
  (save-file (make-pathname `(,(working-path) ,(build-dir)) mfilename) (pp-ix sx))
  (set! pstate ((.~ sx (keyw :mfile)) pstate)))

(define (save-dfile sx)
  (when (not (in-project)) (error "cannot save dfile outside of project"))
  (save-file (make-pathname `(,(working-path) ,(build-dir)) dfilename) (pp-ix sx))
  (set! pstate ((.~ sx (keyw :dfile)) pstate)))

)
