(module command.build.compiler (compile)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.process)
(import chicken.process-context)
(import chicken.pathname)

(import tabulae)
(import ix)

(import (prefix state state:))
(import util)
(import graph)

(define env "/usr/bin/env")

; XXX ask about -no-module-registration text on wiki says it can be used if making your own import libraries?
; these are all functions because pwd needs to happen after state:init
; XXX include-path is specifically for include, there doesn't appear to be a way to define alternative extension repos
(define (pwd) (make-pathname (state:working-path) (state:build-dir)))
(define (cscflags) `("-setup-mode" "-include-path" ,(pwd)))
(define (cflags) `("-c" "-fno-strict-aliasing" "-fwrapv" "-DHAVE_CHICKEN_CONFIG_H" "-DC_ENABLE_PTABLES"
                 "-O2" "-fomit-frame-pointer" "-fPIC" "-DPIC" ,(<> "-I" (pwd)) "-I/usr/include/chicken"))
(define (ldflags) `(,(<> "-L" (pwd))  "-L/usr/lib" "-L/usr/local/lib"
                  ,(<> "-Wl,-R" (pwd)) "-Wl,-R/usr/lib" "-Wl,-R/usr/local/lib"))

(define module->unit symbol->string)

(define (module->cfile tag #!optional static)
  (let ((base (if static (<> (module->unit tag) ".static") (module->unit tag))))
       (make-pathname #f base "c")))

(define (module->ofile tag #!optional static)
  (pathname-replace-extension (module->cfile tag static) "o"))

(define (module->inline tag)
  (make-pathname #f (module->unit tag) "inline"))

(define (module->types tag)
  (make-pathname #f (module->unit tag) "types"))

(define (module->link tag)
  (make-pathname #f (module->unit tag) "link"))

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
  (define linkfile (module->link tag))
  (define user-flags (map ix:unwrap! ((^.!! (keyw :csc-flags)) (state:pfile))))

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

  ; XXX not clear on whether these things should be set for toplevel or all modules
  ; XXX I don't actually know if the linkfile is used by chicken, or if it's just for my convenience if I need it
  (define toplevel-clauses (cond ((and dynamic library is-root)
                                  `("-dynamic" "-feature" "chicken-compile-shared"))
                                 ((and static library is-root)
                                  `("-emit-link-file" ,linkfile))
                                 (else '())))

  (define static-clauses (if static
                             `("-static" "-module-registration")
                             '()))

  (define args `("chicken" ,infile "-output-file" ,outfile ,@(cscflags) ,@user-flags
                 ,@unit-clauses ,@uses-clauses ,@toplevel-clauses ,@static-clauses))

  (when verbose (printf "~A\n\n" (string-intersperse (cons env args))))
  (process-run env args))

; compile a single c to o
(define (cc tag #!key is-root library static verbose)
  (define dynamic (not static))
  (define cc (symbol->string ((^.!! (keyw :cc)) (state:pfile))))
  (define infile (module->cfile tag static))
  (define outfile (module->ofile tag static))

  ; XXX do I only want this at the toplevel?
  ; XXX also chicken-install doesn't set -fPIC or -DPIC on static builds, why?
  (define shared-clause (if (and dynamic library) `("-DC_SHARED") '()))

  (define args `(,cc ,infile "-o" ,outfile ,@(cflags) ,@shared-clause))

  (when verbose (printf "~A\n\n" (string-intersperse (cons env args))))
  (process-run env args))

; link all. this is never called for static libraries
(define (ld module-tags artifact-tag #!key library static verbose)
  (define cc (symbol->string ((^.!! (keyw :cc)) (state:pfile))))
  (define ld (symbol->string ((^.!! (keyw :ld)) (state:pfile))))
  (define infiles (map (lambda (tag) (module->ofile tag static)) module-tags))
  (define outfile (let ((basename (symbol->string artifact-tag)))
                       (if library (make-pathname #f basename "so") basename)))

  (define shared-clauses (if library `("-shared") '()))
  (define link-clauses (if static `("-static" "-l:libchicken.a") `("-lchicken")))

  ; this is extremely dumb but I don't think I can use chicken's link files without writing a parser for egg files
  (define egg-infiles (if static (glob (make-pathname "eggs" "*.o")) '()))

  (define args `(,cc ,@infiles ,@egg-infiles "-o" ,outfile ,(<> "-fuse-ld=" ld) ,@(ldflags)
                 ,@shared-clauses ,@link-clauses "-lm" "-ldl"))

  (when verbose (printf "~A\n\n" (string-intersperse (cons env args))))
  (process-run env args))

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
                                                 (csc tag ((^.!! (keyw :path)) m) :is-root ((^.!! (keyw :is-root)) m)
                                                      :local-imports (second* leaf) :library library
                                                      :static static :verbose verbose)))
                            leaves))
             (_ (for-each process-join csc-pids))
             (cc-pids (map (lambda (leaf) (let* ((tag (first* leaf))
                                                 (m (tag->module modules tag)))
                                                (cc tag :is-root ((^.!! (keyw :is-root)) m) :library library
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

; XXX TODO FIXME we need to pass deps to ld when building statically, figure this out as I implement pontiff gather
(define (compile modules artifact static verbose)
  (define filtered-adjlist (map module->adjlist (filter-skippable modules)))
  (define all-module-tags (map (^.!! (keyw :name)) modules))
  (define library (library? artifact))
  (define dynamic (not static))

  (change-directory (state:build-dir))

  (define cc-pids (compile-loop filtered-adjlist modules '() artifact static verbose))
  (printf "* csc done\n")

  (for-each process-join cc-pids)
  (printf "* cc done\n")

  ; XXX chicken-install never calls ld for staticlibs ... does chicken pull in extensions automatically? does it for exes?
  ; ld complains about not finding a start symbol with static lib link, so probably unnecessary. need to test more
  ; XXX UPDATE I am fairly sure the right thing is to link everything except static libs
  ; thus linking a dynamic exe or lib only involves its local modules, with eggs and deps resolved at runtime
  ; whereas linking a static exe we need to pull in *all* static object files for *every* dep/egg
  ; this is extremely annoying... this must also be why csc naively creates shared objects for every single module
  ; I... hm I think I can just create archive files, why do I need to copy what csc does?
  (cond ((and static library) (printf "TODO ar rcs artname.a mod1.static.o mod2.static.o\n"))
        (else (process-join (ld all-module-tags ((^.!! (keyw :name)) artifact)
                                :library library :static static :verbose verbose))
              (printf "* ld done\n")))

  (when (and dynamic library)
        (compile-import-so ((^.!! (keyw :root)) artifact) verbose)
        (printf "* import library done\n"))

  (change-directory (state:working-path)))

)
