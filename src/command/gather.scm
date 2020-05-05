(module command.gather (gather)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.file.posix)
(import chicken.process-context)
(import chicken.pathname)

(import tabulae)
(import tabulae.monad)
(import ix)

(import (prefix state state:))
(import util)

; XXX ok take three start thinking fresh
; envs I need to care about CHICKEN_EGG_CACHE, CHICKEN_INSTALL_REPOSITORY, and CHICKEN_REPOSITORY_PATH
; work should have a deps dir and an eggs dir
; * clone/curl all deps, recursively. this function will be structured much like load-all-modules
; * map all (project and deps) pontiff files to their egg lists, flatten and nub
; * chicken-install all eggs, using my eggs dir as cache, install repo, and repo path
; * build and "install" deps in order, using base of the deps dir as the target
;   install entails copying the so, link, import so, and pontiff files, and renaming the static toplevel to just o
;   always build with --all and copy all lib artifacts. note dep list specifies project, not artifact
;   eg, you require tabulae, build all results incidentally in parsec and monad which may be imported with no further spec
; * write out a projname-deps.ix file that lists the eggs and pontiff deps we have fetched
;   then next init call we only fetch new deps
; oh actually I can strip project name out of modules file if I'm not merging work dirs
; I still need a deps file for static linking. also I think I can build pontiff deps in parallel
; the way static linking actually appears to work is the chicken -static flag says like
; "we are going to link in the missing units when we build our finished executable"
; I think. chicken compiler may pull in eggs somehow but I... think it doesn't
; in theory, building a lib/exe dynamic it expects to find sos at runtime... wait no it still expects to see import sos
; anyway with static I think it defers that and you just pull everything together in linking
; this is so messy, wild random guessing. grepping through chicken-core is only moderately helpful tho
; just need to test systematically

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

; XXX TODO I actually do want to write out the deplists, not to skip walking, but for the benefit of static linking
(define (gather argv)
  (define verbose ((^.!! (keyw :verbose)) argv))
  ; this clones, symlinks, or whatever every dependency, every dependency's dependency, etc
  ; then returns two lists of symbols as specified
  (printf "fetching dependencies\n")
  (define eggs/deps (fetch-all-deps (access-dlist :dependencies (state:pfile))
                                    (access-dlist :egg-dependencies (state:pfile))
                                    '()))
  ; next we install all eggs locally to this project
  (printf "compiling eggs\n")
  (process-join (process-create (string-intersperse `("/usr/bin/env" "chicken-install" ,@(map symbol->string (car eggs/deps))
                                                      ,@(if verbose '() `("2>&1 | sed -n 's/^building.*/\\* &/p'"))))
                                #f
                                (state:env)))
  (printf "compiling dependencies\n")
  (for-each (lambda (name) (let ((dpath (make-pathname `(,(state:working-path) ,(state:build-dir) "deps")
                                                       (symbol->string name))))
                                (change-directory dpath)
                                ; XXX TODO pass through a nogather flag once I add that
                                (process-join (process-create "/usr/bin/env"
                                                              `("pontiff" "build" "--all" ,@(if verbose `("--verbose") '()))
                                                              (state:env)))
                                ; XXX TODO I also need to symlink the resulting artifacts back to the parent
                                ; XXX also also for this to actually work without adding to repo path I need a static pontiff
                                (change-directory (state:working-path))))
            (cdr eggs/deps))
  ; XXX TODO write out a deps file for linking
  (printf "gather complete\n"))

)
