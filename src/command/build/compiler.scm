(module command.build.compiler (compile)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
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
  (define cc (symbol->string ((^.!! (keyw :cc)) (state:pfile))))
  (define infile (module->cfile tag static))
  (define outfile (module->ofile tag static))

  ; XXX again unclear if needed for static library or just dynamic
  ; I'm pretty sure my confusion is pontiff1 I wrote this to mean a statically-linked shared object
  ; whereas we actually want to this to be an object file suitable for compiling into a static executable
  ; XXX shared doesn't mean dynamic, this makes a fake entrypoint lol
  (define shared-clause (if library `("-DC_SHARED") '()))

  (define args `(,cc ,infile "-o" ,outfile ,@(cflags) ,@shared-clause))

  (when verbose (printf "~A\n\n" (string-intersperse (cons env args))))
  (process-run env args))

; link all
(define (ld module-tags artifact-tag #!key library static verbose)
  (define cc (symbol->string ((^.!! (keyw :cc)) (state:pfile))))
  (define ld (symbol->string ((^.!! (keyw :ld)) (state:pfile))))
  (define infiles (map module->ofile module-tags))
  (define outfile (let ((basename (symbol->string artifact-tag)))
                       (cond ((and library static) (make-pathname #f basename "static.o"))
                             (library (make-pathname #f basename "so"))
                             (else basename))))

  ; XXX again almost certainly nix shared for static lib
  (define shared-clauses (if library `("-shared") '()))
  (define link-clauses (if static `("-static" "-l:libchicken.a") `("-lchicken")))

  (define args `(,cc ,@infiles "-o" ,outfile ,(<> "-fuse-ld=" ld) ,@(ldflags)
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

; XXX ok wtf do I have to do for this shit
; this only gets called for a dynamic library
; it's compiled like a normal so. no unit... toplevel takes care of that
; no uses, pass in no imports. include build dir in path
(define (compile-import-so tag verbose)
  (define import-tag (symbol-append tag '.import))
  (define import-file (make-pathname (state:build-dir) (symbol->string import-tag) "scm"))

  (process-join (csc import-tag import-file :is-root #t :is-module #f :library #t :static #f :verbose verbose))
  (process-join (cc import-tag :is-root #t :library #t :static #f :verbose verbose))
  (process-join (ld `(,import-tag) import-tag :library #t :static #f :verbose verbose)))

; XXX TODO FIXME we need to pass deps to ld when building statically, figure this out as I implement pontiff init
; XXX TODO FIXME testing with a trivial lib/exe pair, I need to be in .pontiff-work when running exe for it to pick up lib
(define (compile modules artifact static verbose)
  (define adjlist (map module->adjlist modules))
  (define module-tags (map car adjlist))
  (define library (library? artifact))
  (define dynamic (not static))

  (change-directory (state:build-dir))

  (define cc-pids (compile-loop adjlist modules '() artifact static verbose))
  (printf "* csc done\n")

  (for-each process-join cc-pids)
  (printf "* cc done\n")

  ; XXX chicken-install never calls ld for staticlibs ... does chicken pull in extensions automatically? does it for exes?
  ; ld complains about not finding a start symbol with static lib link, so probably unnecessary. need to test more
  (process-join (ld module-tags ((^.!! (keyw :name)) artifact) :library library :static static :verbose verbose))
  (printf "* ld done\n")

  (when (and dynamic library)
        (compile-import-so ((^.!! (keyw :root)) artifact) verbose)
        (printf "* import library done\n"))

  (change-directory (state:working-path)))

)
