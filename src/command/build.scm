(module command.build (build build-artifact)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.pathname)
(import chicken.condition)
(import chicken.sort)

(import tabulae)
(import tabulae.monad)
(import ix)
(import simple-sha1)

(import (prefix state state:))
(import util)
(import graph)
(import (prefix command.gather command:))
(import command.build.compiler)

; x.y.z -> (x y z) and vice versa
(define (module-split m) (map string->symbol (string-split (symbol->string m) ".")))
(define (module-join m) (string->symbol (string-intersperse (map symbol->string m) ".")))

; is sm (nonstrictly) a submodule of m?
(define (module-of m sm)
  (let ((m^ (module-split m))
        (sm^ (module-split sm)))
       (and (<= (length m^) (length sm^))
            (equal? (take* (length m^) sm^) m^))))

; test.a.b -> "test/a/b.scm"
; x.y.z -> "src/x/y/z.scm"
(define (module-path m)
  (let* ((mm-str (map symbol->string (module-split m)))
         (dir (if (equal? (car mm-str) "test")
                  (cons ((^.!! (keyw :test-dir)) (state:pfile)) (drop-right* 1 (cdr mm-str)))
                  (cons ((^.!! (keyw :source-dir)) (state:pfile)) (drop-right* 1 mm-str)))))
        (make-pathname dir (car (take-right* 1 mm-str)) "scm")))

; check an adjlist for cycles while build up subgraph hashes via iterative cutleaves calls
; returned list is ordered leaves-first
(define (sort-dag adjl objs acc)
  (let* ((parts (cutleaves adjl))
         (leaves (first* parts))
         (branches (second* parts))
         ; takes a vertex/edge pair from the adjlist, locates the object for the vertex
         ; gathers subgraph hashes for all imports, assigns the vertex a subgraph hash
         ; defined as hash(file hash || all import subgraph hashes sorted by name)
         (mk-leaf-obj (lambda (v/e)
           (let* ((pmodule (tag->module objs (first* v/e)))
                  (sg-hashes (map (lambda (e) ((^.!! (keyw :subgraph-hash)) (tag->module acc e)))
                                  (sort (second* v/e) (lambda (s1 s2) (string<? (symbol->string s1) (symbol->string s2))))))
                  (sg-hash (<> "sha1:" (string->sha1sum (apply <> `(,((^.!! (keyw :file-hash)) pmodule) ,@sg-hashes))))))
                 ((.~! sg-hash (keyw :subgraph-hash)) pmodule)))))
        (cond ((null? adjl) acc)
              ((null? leaves) (die "could not remove further leaves, remaining graph is cyclic: ~S" (map car adjl)))
              (else (sort-dag branches objs (<> acc (map mk-leaf-obj leaves)))))))

; after a build we write the module list to disk
; this loads it if it exists and marks as skippable subgraphs unchanged since the previous build
; the map should be read "mark as skippable if any in the mlist matches on all of name, is-root, and subgraph-hash
(define (skip-subgraphs objs mlist)
  (let ((match-kw (lambda (kw o1 o2) (equal? ((^. (keyw kw)) o1) ((^. (keyw kw)) o2)))))
       (map (lambda (mutandum) ((.~! (any* (lambda (model) (and (match-kw :name mutandum model)
                                                                (match-kw :is-root mutandum model)
                                                                (match-kw :subgraph-hash mutandum model)))
                                           mlist)
                                     (keyw :skip-compile))
                                mutandum))
            objs)))

(define (load-module m)
  (letrec ((path (module-path m))
           ; straightforward
           (catch-read (lambda (port)
             (condition-case (read port)
               (e () (die "read error in ~S" m)))))
           ; XXX I don't fully understand the syntax import forms, I think I can emit imports but not compile or link them?
           ; I don't use or understand this feature so I'm ignoring it for now but it's a potential optimization later
           ; chicken's module system is somewhat arcane, with all the unit and extension and load library etc legacy stuff
           (isimport? (lambda (sx)
             (and (list? sx)
                  (not (null? sx))
                  (memq (car sx) `(import import-syntax import-for-syntax import-syntax-for-syntax reexport)))))
           ; a single spec, not an import statement as a whole
           (parse-import-spec (lambda (i)
             (cond ((symbol? i) i)
                   ((and (list? i) (not (null? i)) (memq (car i) `(only except rename prefix)))
                    (parse-import-spec (second* i)))
                   ((and (list? i) (not (null? i)) (all* symbol? i))
                    (module-join i))
                   (else (die "malformed import spec in ~S" m)))))
           ; sanity check the module spec, extract all imported module names, package as pontiff:module 
           (parse-module-body (lambda (o)
             (let ((name (if (list? (second* o)) (module-join (second* o)) (second* o)))
                   (exports (third* o))
                   (body (cdddr o)))
                  (when (not (eq? name m)) (die "expected module name \"~S\" but got \"~S\"" m name))
                  (when (eq? exports '=) (die "functors are not supported"))
                  (when (not (or (eq? exports '*) (and (list? exports) (all* symbol? exports)))) (die "bad export specifier"))
                  (when (and (= (length body) 1) (string? (car body))) (die "whole-module include not supported"))
                  ; so this filters to a list of imports, then maps a function over the imports that maps over the import specs
                  ; producing a list of list of symbols, flatten and dedupe (tho really there should be no duplicates)
             (let* ((imports (nub* (flatten (map (lambda (i) (map parse-import-spec (cdr i)))
                                                 (filter* isimport? body)))))
                    (system-libs (filter* (lambda (i) (or (memq i '(scheme r5rs r4rs srfi-4)) (module-of 'chicken i))) imports))
                    ; XXX FIXME this creates the annoying situation that the dep name must match the imported module names
                    ; perhaps gather should pull down a list of library artifact roots and store that somewhere?
                    (dep-names (map (^.!! (keyw :name)) ((^.!! (keyw :dependencies)) (state:pfile))))
                    (pontiff-libs (filter* (lambda (i) (any* (lambda (d) (module-of d i)) dep-names)) imports))
                    (egg-names (map ix:unwrap! ((^.!! (keyw :egg-dependencies)) (state:pfile))))
                    (egg-libs (filter* (lambda (i) (any* (lambda (e) (module-of e i)) egg-names)) imports))
                    ; I don't do anything with the other lists but could be nice to sanity check things?
                    (local-imports (difference* imports system-libs pontiff-libs egg-libs)))
                   (ix:build! 'pontiff:module :name m :path path :file-hash (<> "sha1:" (sha1sum (module-path m)))
                                              :subgraph-hash "" :is-root #f :skip-compile #f :local-imports local-imports)))))
           ; a file should be nothing but compiler declarations and a single module
           ; the core constraint of pontiff is that one file = one module = one compilation unit
           (read-file (lambda (port)
             (let ((o (catch-read port)))
                  (when (eof-object? o) (die "unexpected eof in ~S" m))
                  (case (and (list? o) (not (null? o)) (car o))
                    ((declare) (read-file port))
                    ((module) (let ((b (parse-module-body o)))
                                   (when (not (eof-object? (catch-read port))) (die "unexpected toplevel after module in ~S" m))
                                   b))
                    ((functor) (die "functors are not supported"))
                    ((include include-relative) (die "toplevel includes are not supported but might be if there's a usecase"))
                    ((require-library require-extension) (die "require isn't supported and tbh idk what it's even for"))
                    (else (die "unexpected toplevel before module in ~S" m)))))))
          (if (and (file-exists? path) (file-readable? path))
              (call-with-input-file path read-file)
              (die "could not load module ~S at path ~A" m path))))

; to-load is a list of tags, loaded is our accumulator of parsed objects
; the trick here is we take one item to load at a time, load it, extract its imports
; then our next to-load list is the union of what's left plus the difference of the extracted imports and the past loads
; and in this way we work through the flattened graph without repeats or omissions
(define (load-all-modules to-load loaded)
  (if (null? to-load)
      ((.~! #t (idx 0) (keyw :is-root)) (reverse loaded))
      (let* ((m (load-module (car to-load)))
             (m-imports (map ix:unwrap! ((^.!! (keyw :local-imports)) m)))
             (loaded^ (cons m loaded))
             (to-load^ (union* (cdr to-load) (difference* m-imports (map (^.!! (keyw :name)) loaded^)))))
            (load-all-modules to-load^ loaded^))))

; build a single artifact
(define (build-artifact artifact #!optional verbose static dry-run force-build)
  (define aname ((^.!! (keyw :name)) artifact))
  (define aroot ((^.!! (keyw :root)) artifact))
  (define build-dynamic (or (library? artifact) (not static)))
  (define build-static (or (library? artifact) static))

  (printf "building ~S\n" aname)

  ; one ix object per module with name/root/imports
  (printf "* preparing modules... ")
  (define base-modules (load-all-modules `(,aroot) '()))
  (printf "done\n")

  ; same modules objects, sorted leaves-first, with computed subgraph hashes
  (printf "* checking module graph... ")
  (define sorted-modules (sort-dag (map module->adjlist base-modules) base-modules '()))
  (printf "done\n")

  ; filter out modules whose local subgraphs have not changed
  ; remember, we build two artifacts for libraries but one for executables
  ; XXX TODO FIXME this is fundamentally flawed for multi-artifact projects!!
  ; what I actually want is a freeform ix object wth every module/static/root triple a key, sg hash a value
  ; we can't just replace unfiltered lists, otherwise we lose data on other artifacts' modules
  ; XXX FIXME I also need to track whether we have last built a static or dynamic executable, or else always at least link
  (printf "* determining build order... ")
  (define dyn-modules (cond ((and build-dynamic force-build) sorted-modules)
                            (build-dynamic (skip-subgraphs sorted-modules ((^.!! (keyw :dynamic)) (state:mfile))))
                            (else '())))

  (define stat-modules (cond ((and build-static force-build) sorted-modules)
                             (build-static (skip-subgraphs sorted-modules ((^.!! (keyw :static)) (state:mfile))))
                             (else '())))

  (define do-dyn (not (null? (filter-skippable dyn-modules))))
  (define do-stat (not (null? (filter-skippable stat-modules))))
  (printf "done\n")

  ; note we use the filtered list in compile and write the unfiltered list to modules.ix
  (cond (dry-run (printf "~S: dryrun finished\n\n" aname))
        ((and (not do-dyn) (not do-stat)) (printf "~S: nothing to do\n" aname))
        (else (when do-dyn
                    (printf "compiling ~S/~S modules (dynamic)\n"
                            (length (filter-skippable dyn-modules))
                            (length dyn-modules))
                    (compile dyn-modules artifact #f verbose)
                    (printf "compilation finished\n")
                    (state:save-mfile ((.~! (ix:wrap 'list dyn-modules) (keyw :dynamic)) (state:mfile))))
              (when do-stat
                    (printf "compiling ~S/~S modules (static)\n"
                            (length (filter-skippable stat-modules))
                            (length stat-modules))
                    (compile stat-modules artifact #t verbose)
                    (printf "compilation finished\n")
                    (state:save-mfile ((.~! (ix:wrap 'list stat-modules) (keyw :static)) (state:mfile))))
              (printf "~S build finished\n" aname))))

(define (build argv)
  (define verbose ((^.!! (keyw :verbose)) argv))
  (define static ((^.!! (keyw :static)) argv))
  (define dry-run ((^.!! (keyw :dry-run)) argv))
  (define force-build ((^.!! (keyw :force)) argv))

  ; gather deps for all artifacts
  (when ((^.!! (keyw :gather)) argv)
        (command:gather (ix:build! 'pontiff:gather:argv :verbose verbose)))

  ; build each artifact in turn
  (for-each (lambda (a) (build-artifact a verbose static dry-run force-build))
            ((^.!! (keyw :artifacts)) argv))
  (printf "all builds complete\n"))

)
