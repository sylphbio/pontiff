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
(import graph)

; dumb convenience function
(define (access-dlist kw sx)
  (let ((dl ((^. (keyw kw)) sx)))
       (map (lambda (d) (if (ix:symbol? d) (ix:unwrap d) d))
            (if dl (ix:unwrap dl) '()))))

; very dumb convenience function
(define (dname d)
  (cond ((ix:sexp? d) ((^.v (keyw :name)) d))
        ((symbol? d) d)
        ; XXX TODO symbol plus version constraint
        (else (die "cannot get name of unknown dependency form: ~S" d))))

; compare names of two dependencies of any form
; different forms of the same named dependency are *equivalent* dependencies
; it is a crucial assumption (which we in theory enforce) that one and omly one version of each dependency exists
(define (dep=? d1 d2)
  (eq? (dname d1) (dname d2)))

; XXX FIXME aaaaa I wanted to keep the filename in state but I have no mechanism to load a pfile not for the current project
; this should be cleaned up later but it is entirely a case of ritual propriety, not shoddiness
(define (load-pfile name dir)
  (ix:validate-as 'pontiff (parse:ix (load-file (make-pathname dir "pontiff" "ix")))))

; takes a pontiff:repository object, clones it, parses and returns its pontiff file
(define (clone-repo r dir)
  (let* ((name ((^.v (keyw :name)) r))
         (vcs ((^.v (keyw :vcs)) r))
         (url ((^.v (keyw :url)) r)))
        (case vcs
          ((git) (process-join (process-create "/usr/bin/env" `("git" "clone" ,url ,dir))))
          (else (die "unknown or unimplemented vcs ~S" vcs)))
        (load-pfile name dir)))

; takes a pontiff:filepath and symlinks the directory, parses and returns pontiff file
(define (link-directory p dst)
  (let* ((name ((^.v (keyw :name)) p))
         (src ((^.v (keyw :path)) p)))
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
        (when (not (dep=? ((^.v (keyw :name)) pfile) d)) (die "name mismatch for dependency ~S" d))
        pfile))

; this works much like build's load-all-modules, memoized breadth-first graph traversal
; returns a list of eggs, adjlist of pontiff dependencies, list of linkable libraries
; note because this dedupes top-down you can actually use the pontiff file to do dependency injection
(define (fetch-all-deps to-load egg-list dep-adjlist lib-list)
  (if (null? to-load)
      `(,egg-list ,dep-adjlist ,lib-list)
      (let* ((pfile (fetch-dep (car to-load)))
             (new-eggs (access-dlist :egg-dependencies pfile))
             (new-deps (access-dlist :dependencies pfile))
             (new-libs (access-dlist :lib-dependencies pfile))
             (eggs^ (union* egg-list new-eggs))
             ; this is a simple adjlist
             (deps^ `((,((^.v (keyw :name)) pfile) ,(map dname new-deps)) ,@dep-adjlist))
             (libs^ (union* lib-list new-libs))
             ; this however preserves the full pfile component, needed by fetch-dep to determine how to fetch it
             (to-load^ (union-by* dep=? (cdr to-load) (difference-by* dep=? new-deps (map car dep-adjlist)))))
            (fetch-all-deps to-load^ eggs^ deps^ libs^))))

; chicken-install any eggs our project needs
(define (gather-eggs eggs verbose)
  (define cmd (string-intersperse `("/usr/bin/env" "chicken-install" ,@(map symbol->string eggs)
                                    ,@(if verbose '() `("2>&1 | sed -n 's/^building.*/\\* &/p'")))))
  (printf "setting up eggs\n")
  ; note this is a shellout just to make filtering stdout reasonable
  (when verbose (printf "~A\n\n" cmd))
  (process-join (process-create cmd #f (state:env))))

; builds previously fetched pontiff dependencies and symlinks artifacts up into the shared deps dir
(define (gather-deps deps verbose)
  (printf "compiling dependencies\n")
  (for-each (lambda (name)
    (let ((dpath (make-pathname `(,(state:working-path) ,(state:build-dir) "deps") (symbol->string name))))
         (when verbose (printf "cd ~A\n" dpath))
         (change-directory dpath)
         (define build-exe (executable-pathname))
         (define build-args `("build" "--all-libs" "--no-gather" ,@(if verbose `("--verbose") '())))
         (when verbose (printf (<> (string-intersperse (cons build-exe build-args)) "\n")))
         ; note for subinvoke we always use whatever binary this is
         (process-join (process-create build-exe
                                       build-args
                                       (cons `("PONTIFF_SUBINVOCATION" . "1") (state:env))))
         (for-each (lambda (src) (let ((dst (normalize-pathname (make-pathname `(,dpath "..") (pathname-strip-directory src)))))
                                      (when (not (file-exists? dst)) (create-symbolic-link src dst))))
                   (glob (make-pathname `(,dpath ,(state:build-dir)) "*.so")
                         (make-pathname `(,dpath ,(state:build-dir)) "*.a")))
         (change-directory (state:working-path))))
    deps))

; the basic flow here is we recursively clone/curl/link our project's pontiff deps, their deps, etc
; nub out a flat list of eggs, sort a dag of all pontiff deps
; install all the eggs locally to the project. chicken-install handles eggs depended on by eggs
; it does annoyingly force us to link all eggs to all artifacts however, since we don't want to touch egg info files
; then with eggs in place we can build our pontiff deps and link the artifacts in a central location
; XXX I could build pontiff dep leaf layers in parallel but I won't bother until the ecosystem is bigger
(define (gather argv)
  (define gather-all ((^.v (keyw :all)) argv))
  (define verbose ((^.v (keyw :verbose)) argv))
  (define force-gather #f) ; XXX TODO impl this

  ; fetch pontiff dependencies recursively, returning a pair of a list of egg names and dep names
  ; we dedupe unconditionally to prevent heisenbugs but really you should not have duplicates
  (printf "checking dependencies\n")
  (define pdeps (union-by* dep=? (access-dlist :dependencies (state:pfile))
                                 (if gather-all (access-dlist :test-dependencies (state:pfile)) '())))
  (define edeps (union-by* dep=? (access-dlist :egg-dependencies (state:pfile))
                                 (if gather-all (access-dlist :egg-test-dependencies (state:pfile)) '())))
  (define e/d/l (fetch-all-deps pdeps edeps '() (access-dlist :lib-dependencies (state:pfile))))

  ; figure out what eggs and deps we actually need to get
  ; again, required-* is the intersection of all pontiff files' declarations
  (define required-eggs (first* e/d/l))
  (define required-deps (let ((d (sort-dag (second* e/d/l))))
                             (if d (map car d) (die "cycle(s) detected in dependency graph: ~S" (second* e/d/l)))))
  (define required-libs (third* e/d/l))

  (define existing-eggs (map ix:unwrap ((^.v (keyw :eggs)) (state:dfile))))
  (define existing-deps (map ix:unwrap ((^.v (keyw :deps)) (state:dfile))))

  (define missing-eggs (difference* required-eggs existing-eggs))
  (define missing-deps (difference* required-deps existing-deps))

  ; and now the rules are, if forced we get all required, else we get all missing if nonempty
  (define do-eggs (if force-gather required-eggs missing-eggs))
  (define do-deps (if force-gather required-deps missing-deps))

  ; next install all eggs locally to this project
  (when (not (null? do-eggs)) (gather-eggs do-eggs verbose))
  ; sorry my lenses still aren't perfect
  (state:save-dfile ((.~ (ix:wrap 'list (map (lambda (e) (ix:wrap 'symbol e)) required-eggs)) (keyw :eggs)) (state:dfile)))

  ; then build all pontiff dependencies
  (when (not (null? do-deps)) (gather-deps do-deps verbose))
  (state:save-dfile ((.~ (ix:wrap 'list (map (lambda (d) (ix:wrap 'symbol d)) required-deps)) (keyw :deps)) (state:dfile)))

  ; and write out libs, compile driver will link as necessary
  (state:save-dfile ((.~ (ix:wrap 'list (map (lambda (d) (ix:wrap 'symbol d)) required-libs)) (keyw :libs)) (state:dfile)))

  (when (or (not (null? do-eggs)) (not (null? do-deps))) (printf "gather complete\n")))

)
