(module command.build.compiler (compile)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.foreign)
(import chicken.process)
(import chicken.process-context)
(import chicken.pathname)

(import tabulae)
(import ix)

(import (prefix state state:))
(import (prefix command.build.template template:))
(import util)
(import graph)

(define binenv "/usr/bin/env")

(define (prologue-path)
  (make-pathname (state:link-path) "pontiff-prologue" "scm"))

(define (lpath #!optional subdir)
  (if subdir
      (make-pathname (state:link-path) subdir)
      (state:link-path)))

(define (ld-lpaths prefix)
  (map ((curry* <>) prefix) `(,(lpath) ,(lpath "deps") ,(lpath "eggs") ,(lpath "sys"))))

(define (cscflags) `("-include-path" ,(lpath) "-include-path" ,(lpath "deps") "-include-path" ,(lpath "eggs")
                     "-include-path" ,(lpath "sys") "-include-path" ,(lpath "include")))

(define (cflags) `("-c" "-fno-strict-aliasing" "-fwrapv" "-DHAVE_CHICKEN_CONFIG_H" "-DC_ENABLE_PTABLES"
                   "-O2" "-fomit-frame-pointer" "-fPIC" "-DPIC"
                   ,(<> "-I" (lpath "include"))
                   ,(<> "-I" (foreign-value C_TARGET_INCLUDE_HOME c-string))))

(define (ldflags) `(,@(ld-lpaths "-L") "-L/usr/lib" "-L/usr/local/lib"
                    ,@(ld-lpaths "-Wl,-R") "-Wl,-R/usr/lib" "-Wl,-R/usr/local/lib"
                    ,(<> "-L" (foreign-value C_TARGET_LIB_HOME c-string))
                    ,(<> "-Wl,-R" (foreign-value C_TARGET_LIB_HOME c-string))))

(define module->unit symbol->string)

(define (module->cfile tag #!optional static)
  (let ((base (if static (<> (module->unit tag) "##static") (module->unit tag))))
       (make-pathname #f base "c")))

(define (module->ofile tag #!optional static)
  (pathname-replace-extension (module->cfile tag static) "o"))

(define (module->inline tag)
  (make-pathname #f (module->unit tag) "inline"))

(define (module->types tag)
  (make-pathname #f (module->unit tag) "types"))

; compile a single scm to c
(define (csc tag path #!key is-root (local-imports '()) (is-module #t) library static verbose)
  (define executable (not library))
  (define dynamic (not static))
  (define module-name (symbol->string tag))
  (define unit-name (module->unit tag))
  (define infile (make-pathname (state:working-path) path))
  (define outfile (module->cfile tag static))
  (define inlinefile (module->inline tag))
  (define typefile (module->types tag))
  (define user-flags (map ix:unwrap ((^.v (keyw :csc-flags)) (state:pfile))))

  ; anything except an exe root is a unit
  (define unit-clauses (if (and is-module (not (and executable is-root)))
                           `("-unit" ,unit-name
                             "-emit-import-library" ,module-name
                             ; XXX doesn't work? "-emit-inline-file" ,inlinefile
                             "-emit-types-file" ,typefile)
                           '()))

  ; technically we only need uses flags for static builds but no harm
  ; XXX I don't know if I need these for nonlocal imports but I don't think so?
  (define uses-clauses (foldl (lambda (acc i) (<> acc
                                                  `("-uses" ,(module->unit i)
                                                    ; XXX emit doesn't work? "-consult-inline-file" ,(module->inline i)
                                                    "-consult-types-file" ,(module->types i))))
                              '()
                              local-imports))

  ; XXX I'm not going to bother setting the chicken features until/unless I find out they actually matter
  ; at least not without a list of what they are
  ; so far I've found: chicken-compile-shared, compiling-extension, compiling-static-extension
  ; XXX UPDATE afaict we're not actually allowed to cond-expand on custom features, these are prolly useless
  (define feature-clauses
    (cond ((and dynamic library is-root) `("-feature" "pontiff-dynamic-library"))
          ((and static library is-root) `("-feature" "pontiff-static-library"))
          ((and dynamic executable is-root) `("-feature" "pontiff-dynamic-executable"))
          ((and static executable is-root) `("-feature" "pontiff-static-executable"))
          (else '())))

  ; this sets repopath for dynamics. eventually could be a staging point for pontiff extensions
  ; I need to think hard and test this tho it is fraught conceptually
  (define prologue-clause `("-prologue" ,(prologue-path)))

  (define dyn/stat-clauses (if static
                               `("-static" "-module-registration")
                               `("-dynamic")))

  (define args `("chicken" ,infile "-output-file" ,outfile ,@user-flags ,@(cscflags)
                 ,@unit-clauses ,@feature-clauses ,@dyn/stat-clauses ,@prologue-clause ,@uses-clauses))

  (when verbose (printf "~A\n\n" (string-intersperse (cons binenv args))))
  (process-run binenv args (state:env)))

; compile a single c to o
(define (cc tag #!key is-root library static verbose)
  (define dynamic (not static))
  (define cc (symbol->string ((^.v (keyw :cc)) (state:pfile))))
  (define infile (module->cfile tag static))
  (define outfile (module->ofile tag static))

  ; XXX do I only want this at the toplevel?
  ; XXX also chicken-install doesn't set -fPIC or -DPIC on static builds, why?
  (define shared-clause (if (and dynamic library) `("-DC_SHARED") '()))

  (define args `(,cc ,infile "-o" ,outfile ,@(cflags) ,@shared-clause))

  (when verbose (printf "~A\n\n" (string-intersperse (cons binenv args))))
  (process-run binenv args))

; combine object files for static libs into a single archive
; this way an importer need know nothing about the internal structure of its imports
(define (ar module-tags artifact-tag #!key verbose)
  (define infiles (map (lambda (tag) (module->ofile tag #t)) module-tags))
  (define outfile (make-pathname #f (symbol->string artifact-tag) "a"))

  (define args `("ar" "rcs" ,outfile ,@infiles))

  (when verbose (printf "~A\n\n" (string-intersperse (cons binenv args))))
  (process-run binenv args))

; link all. this is never called for static libraries
(define (ld module-tags artifact-tag #!key library static verbose)
  (define cc (symbol->string ((^.v (keyw :cc)) (state:pfile))))
  (define ld (symbol->string ((^.v (keyw :ld)) (state:pfile))))
  (define infiles (map (lambda (tag) (module->ofile tag static)) module-tags))
  (define outfile (let ((basename (symbol->string artifact-tag)))
                       (if library (make-pathname #f basename "so") basename)))

  (define shared-clauses (if library `("-shared") '()))
  (define link-clauses (if static `("-static" "-l:libchicken.a") `("-lchicken")))

  ; XXX this is extremely dumb. can I just use the egg/dep imports from load-module?
  ; what I want is to only link things a given artifact uses. need to think about how to turn imports to artifact names tho
  ; note chicken's link files are useless for this purpose
  (define egg-infiles (if static (glob (make-pathname "eggs" "*.o")) '()))
  (define dep-infiles (if static (glob (make-pathname "deps" "*.a")) '()))

  (define args `(,cc ,@infiles ,@egg-infiles ,@dep-infiles "-o" ,outfile ,(<> "-fuse-ld=" ld) ,@(ldflags)
                 ,@shared-clauses ,@link-clauses "-lm" "-ldl"))

  (when verbose (printf "~A\n\n" (string-intersperse (cons binenv args))))
  (process-run binenv args))

; ok basicallly compile loops needs to trim a batch of leaves
; map csc, map wait, map cc, collect the cc pids and repeat
(define (compile-loop adjlist modules acc artifact static verbose)
  (if (null? adjlist)
      acc
      (let* ((part (cutleaves adjlist))
             (leaves (first* part))
             (branches (second* part))
             (library (library? artifact))
             (csc-pids (map (lambda (leaf) (let* ((tag (first* leaf))
                                                  (m (tag->module modules tag)))
                                                 (csc tag ((^.v (keyw :path)) m) :is-root ((^.v (keyw :is-root)) m)
                                                      :local-imports (second* leaf) :library library
                                                      :static static :verbose verbose)))
                            leaves))
             (_ (for-each process-join csc-pids))
             (cc-pids (map (lambda (leaf) (let* ((tag (first* leaf))
                                                 (m (tag->module modules tag)))
                                                (cc tag :is-root ((^.v (keyw :is-root)) m) :library library
                                                        :static static :verbose verbose)))
                           leaves)))
            (compile-loop branches modules (<> acc cc-pids) artifact static verbose))))

; builds an import shared object for a dynamic library
; fairly simple one-file build, no uses no units no imports
(define (compile-import-so tag verbose)
  (define import-tag (symbol-append tag '.import))
  (define import-file (make-pathname (state:build-dir) (symbol->string import-tag) "scm"))
  (process-join (csc import-tag import-file :is-root #t :is-module #f :library #t :static #f :verbose verbose))
  (process-join (cc import-tag :is-root #t :library #t :static #f :verbose verbose))
  (process-join (ld `(,import-tag) import-tag :library #t :static #f :verbose verbose)))

(define (compile modules artifact static verbose)
  (define filtered-adjlist (map module->adjlist (filter-skippable modules)))
  (define all-module-tags (map (^.v (keyw :name)) modules))
  (define library (library? artifact))
  (define dynamic (not static))

  ; move into pontiff work because import files dump to pwd
  (change-directory (state:build-dir))

  ; write out the prologue appended to every file, so far this just fixes the repo path
  (define rlist (sprintf "~S" (string-split (state:repo-path) ":")))
  (define prologue (string-translate* template:prologue `(("##REPOS##" . ,rlist))))
  (save-file (prologue-path) prologue)

  ; main loop that runs csc parallel intrabatch, and cc fully parallel
  (define cc-pids (compile-loop filtered-adjlist modules '() artifact static verbose))
  (printf "* csc done\n")

  ; close out all the cc pids
  (for-each process-join cc-pids)
  (printf "* cc done\n")

  ; for static libraries we bundle everything into archives for convenience, otherwise link
  (cond ((and static library)
         (process-join (ar all-module-tags ((^.v (keyw :name)) artifact) :verbose verbose))
         (printf "* ar done\n"))
        (else
         (process-join (ld all-module-tags ((^.v (keyw :name)) artifact) :library library :static static :verbose verbose))
         (printf "* ld done\n")))

  ; and dynamic libs we also need to build the import lib
  (when (and dynamic library)
        (for-each (lambda (tag) (compile-import-so tag verbose)) all-module-tags)
        (printf "* import libraries done\n"))

  ; done!
  (change-directory (state:working-path)))

)
