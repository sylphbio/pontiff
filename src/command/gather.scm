(module command.gather (gather)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.file.posix)
(import chicken.io)
(import chicken.process)
(import chicken.process-context)
(import chicken.pathname)

(import tabulae)
(import tabulae.monad)
(import ix)

(import (prefix state state:))
(import util)

; XXX OK TOMLORROW NEXT I got static builds working, I fixed a bunch of bugs, so next to finish gather I want to
; * get rid of linkfiles, useless
; * compile should make archives for static libraries
; * gather should symlink all shared objects, archives, and import libs
; * devise mechanism to get pontiff deps into link... probably write artifact-by-artifact deps file with symbol lists
; at this point deps should fully work in theory and we can...
; * impl clean, including depclean
; * split out ix and tabulae
; * impl ix generic and fix modules.ix to use it (also record latest exe type, also rename back to modules.ix)
; * give a pontiff file to uuid
; * add nogather/alllibs flags to build and call gather in build
; * make vc handling its own module with a single generic interface
; * make a separate module from state to contain hardcoded values like pfile/mfile/dfile names etc
; and then... I think we're ready to launch?
; and then I want to impl json conversion in ix for viv and rewrite the sbml parser

; XXX TODO I keep getting confused on why and why not I actually want a deps.ix file
; * record which deps we have actually built. unless forced to refresh we don't want to even attempt rebuilds
;   ideally after the first gather it's totally silent/basically a noop every time you call build unless you change the deplist
;   in which case it would only get the new things that are needed
; * deps.ix would not help with linking, I need to determine what to link on an artifact-by-artifact basis

; dumb convenience function
(define (access-dlist kw sx)
  (map (lambda (d) (if (ix:symbol? d) (ix:unwrap! d) d))
       ((^.!! (keyw kw)) sx)))

; very dumb convenience function
(define (dname d)
  (cond ((ix:sexp? d) ((^.!! (keyw :name)) d))
        ((symbol? d) d)
        (else (die "cannot get name of unknown dependency form: ~S" d))))

; compare names of two dependencies of any form
; different forms of the same named dependency are *equivalent* dependencies
; it is a crucial assumption (which we in theory enforce) that one and omly one version of each dependency exists
(define (dep=? d1 d2)
  (eq? (dname d1) (dname d2)))

; XXX FIXME aaaaa I wanted to keep the filename in state but I have no mechanism to load a pfile not for the current project
; this should be cleaned up later but it is entirely a case of ritual propriety, not shoddiness
(define (load-pfile name dir)
  (let ((pfile (do/m <maybe>
          (>>= (to-maybe (load-file (make-pathname dir "pontiff" "ix")))
                         parse:ix
                         ((curry* ix:validate-as) 'pontiff)))))
       (if (just? pfile)
           (from-just pfile)
           (die "failed to load pontiff.ix for ~S" name))))

; takes a pontiff:repository object, clones it, parses and returns its pontiff file
(define (clone-repo r dir)
  (let* ((name ((^.!! (keyw :name)) r))
         (vcs ((^.!! (keyw :vcs)) r))
         (url ((^.!! (keyw :url)) r)))
        (case vcs
          ((git) (process-join (process-create "/usr/bin/env" `("git" "clone" ,url ,dir))))
          (else (die "unknown or unimplemented vcs ~S" vcs)))
        (load-pfile name dir)))

; takes a pontiff:filepath and symlinks the directory, parses and returns pontiff file
(define (link-directory p dst)
  (let* ((name ((^.!! (keyw :name)) p))
         (src ((^.!! (keyw :path)) p)))
        (when (not (absolute-pathname? src)) (die "dependency path for module ~S must be absolute" name))
        (when (not (directory-exists? src)) (die "no directory ~S for module ~S" src name))
        (create-symbolic-link src dst)
        (load-pfile name dst)))

; based on our chosen resolver, takes a symbol to something that can be cloned or curled
; performs the relevant operation, parses and returns its pontiff file
; XXX TODO FIXME I haven;t decided exactly how I'm going to do this
; I think my minimum effort "alice" resolver will be, it just curls an ix file from my personal domain
; but need to define a prototype etc and rather get basic flow working first
(define (resolve-dep d dir) (error "resolver TODO"))

; given some form of pontiff dep, fetches it into its proper place unless it exists, parses and returns its pontiff file
(define (fetch-dep d)
  (let* ((dir (make-pathname `(,(state:working-path) ,(state:build-dir) "deps") (symbol->string (dname d))))
         (pfile (cond ((directory-exists? dir) (load-pfile (dname d) dir))
                      ((ix:ident=? 'pontiff:repository d) (clone-repo d dir))
                      ((ix:ident=? 'pontiff:directory d) (link-directory d dir))
                      ((symbol? d) (resolve-dep d dir))
                      (else (die "cannot fetch unknown dependency form: ~S" d)))))
        (when (not (dep=? ((^.!! (keyw :name)) pfile) d)) (die "name mismatch for dependency ~S" d))
        pfile))

; this works much like build's load-all-modules, memoized breadth-first graph traversal
; returns two lists of unwrapped symbols
; note because this dedupes top-down you can actually use the pontiff file to do dependency injection
(define (fetch-all-deps to-load eggs loaded)
  (if (null? to-load)
      `(,eggs . ,(reverse loaded))
      (let* ((pfile (fetch-dep (car to-load)))
             (new-eggs (access-dlist :egg-dependencies pfile))
             (new-deps (access-dlist :dependencies pfile))
             (eggs^ (union* eggs new-eggs))
             (loaded^ (cons ((^.!! (keyw :name)) pfile) loaded))
             (to-load^ (union-by* dep=? (cdr to-load) (difference-by* dep=? new-deps loaded^))))
            (fetch-all-deps to-load^ eggs^ loaded^))))

; the basic flow here is we recursively clone/curl/link our project's pontiff deps, their deps, etc
; nub out two flat lists of eggs and deps. chicken-install all the eggs locally to the project
; chicken-install handles missing egg dependencies, so we never need to touch egg files
; it does annoyingly force us to link all eggs to all artifacts however
; then with eggs in place we can build our pontiff deps and link the artifacts in a central location
; unfortunately because chicken needs to see import libraries we have to do this all in serial
(define (gather argv)
  (define verbose ((^.!! (keyw :verbose)) argv))

  ; fetch pontiff dependencies recursively, returning a pair of a list of egg names and dep names
  (printf "fetching dependencies\n")
  (define eggs/deps (fetch-all-deps (access-dlist :dependencies (state:pfile))
                                    (access-dlist :egg-dependencies (state:pfile))
                                    '()))

  ; next we install all eggs locally to this project
  (printf "compiling eggs\n")

  ; should be /usr/lib/chicken/BINVER on normal systems
  ; extremely annoyingly chicken dumps its eggs and its system import libs all in the same directory
  ; we need to symlink system libs, otherwise chicken-install won't install egg dependencies
  ; this all goes away when pontiff manages the compiler and stdlib itself
  (define chicken-repo (call-with-input-pipe "/usr/bin/env chicken-install -repository" read-line))
  (for-each (lambda (src) (let ((dst (make-pathname `(,(state:link-path) "sys")
                                                    (pathname-strip-directory src))))
                               (when (not (file-exists? dst)) (create-symbolic-link src dst))))
            (glob (make-pathname chicken-repo "chicken.*.import.so")
                  (make-pathname chicken-repo "srfi-4.import.so")
                  (make-pathname chicken-repo "types.db")))

  (process-join (process-create (string-intersperse `("/usr/bin/env" "chicken-install" ,@(map symbol->string (car eggs/deps))
                                                      ,@(if verbose '() `("2>&1 | sed -n 's/^building.*/\\* &/p'"))))
                                #f
                                (state:env)))

  ; then build all pontiff dependencies and symlinks artifacts up into the shared deps dir
  (printf "compiling dependencies\n")
  (for-each (lambda (name)
    (let ((dpath (make-pathname `(,(state:working-path) ,(state:build-dir) "deps") (symbol->string name))))
         (change-directory dpath)
         ; XXX TODO FIXME get rid of this call when I hav ebetter moddules.ix
         (process-join (process-create "/usr/bin/env" `("pontiff" "clean" "--depclean")))
         (process-join (process-create "/usr/bin/env"
                                       `("pontiff" "build" "--all-libs" "--no-gather" ,@(if verbose `("--verbose") '()))
                                       (state:env)))
         (for-each (lambda (src) (let ((dst (normalize-pathname (make-pathname `(,dpath "..") (pathname-strip-directory src)))))
                                      (when (not (file-exists? dst)) (create-symbolic-link src dst))))
                   (glob (make-pathname `(,dpath ,(state:build-dir)) "*.so")
                         (make-pathname `(,dpath ,(state:build-dir)) "*.a")))
         (change-directory (state:working-path))))
    (cdr eggs/deps))

  (printf "gather complete\n"))

)
