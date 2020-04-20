(module command.build (build)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.pathname)
(import chicken.condition)

(import tabulae)
(import tabulae.monad)
(import ix)
(import simple-sha1)

(import (prefix state state:))
(import util)

; XXX alright cool dope what the fuck am I doing
; argv is: artifacts, dry-run, static, verbose
; for a normal build of a given artifact (map over) we must:
; * call init if it's never been called
; * read in the root file, check its module name, parse its imports, hash the file contents
; * for every import:
;   - if system, nothing (have a static list)
;   - if foreign, nothing (die if it's supposed to be a submodule that doesn't exist?)
;   - otherwise assume it's local and recurse
;     if there isn't a matching file (module name uniquely determines filename) die
;     think about recursion scheme, we want to only visit each module once
;   we now have an adjlist of local modules and a flat list of foreign modules
; * check for cycles by partitioning leaves until we can't. collect subgraph hashes here
; * abort if dry run
; * drop into compiler TODO determine exact compile/link flow

(define dry-run #f)
(define static #f)
(define verbose #f)

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
                  (cons ((^.!! (keyw :test-dir)) (state:file)) (drop-right* 1 (cdr mm-str)))
                  (cons ((^.!! (keyw :source-dir)) (state:file)) (drop-right* 1 mm-str)))))
        (make-pathname dir (car (take-right* 1 mm-str)) "scm")))

(define (load-module m)
  (letrec ((path (module-path m))
           (hash (sha1sum (module-path m)))
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
                    ; perhaps init should pull down a list of library artifact roots and store that somewhere?
                    (dep-names (map (^.!! (keyw :name)) ((^.!! (keyw :dependencies)) (state:file))))
                    (pontiff-libs (filter* (lambda (i) (any* (lambda (d) (module-of d i)) dep-names)) imports))
                    (egg-names (map ix:unwrap! ((^.!! (keyw :egg-dependencies)) (state:file))))
                    (egg-libs (filter* (lambda (i) (any* (lambda (e) (module-of e i)) egg-names)) imports))
                    ; I don't do anything with the other lists but could be nice to sanity check things?
                    (local-imports (map (lambda (i) (ix:build! 'pontiff:module:import :name i :type 'local))
                                        (difference* imports system-libs pontiff-libs egg-libs))))
                   (ix:build! 'pontiff:module :name m :path path :file-hash hash :subgraph-hash ""
                                              :is-root #f :imports local-imports)))))
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
                    ((require-library require-extension) (die "require isn't supported and tbh I don't know what it's even for"))
                    (else (die "unexpected toplevel before module in ~S" m)))))))
          (if (and (file-exists? path) (file-readable? path))
              (call-with-input-file path read-file)
              (die "could not load module ~S at path ~A" m path))))

; to-load is a list of tags, loaded is our accumulator of parsed objects
(define (load-all-modules to-load loaded)
  (if (null? to-load)
      ((.~! #t (idx 0) (keyw :is-root)) (reverse loaded))
      (let* ((m (load-module (car to-load)))
             (m-imports (map (^.!! (keyw :name)) ((^.!! (keyw :imports)) m)))
             (loaded^ (cons m loaded))
             (to-load^ (union* (cdr to-load) (difference* m-imports (map (^.!! (keyw :name)) loaded^)))))
           (load-all-modules to-load^ loaded^))))

(define (build-artifact artifact)
  (define m (load-all-modules `(,((^.!! (keyw :root)) artifact)) '()))
  (printf "MAZ m:\n  ~A\n" (string-intersperse (map stringify:ix m) "\n  ")))

(define (build argv)
  (set! dry-run ((^.!! (keyw :dry-run)) argv))
  (set! static ((^.!! (keyw :static)) argv))
  (set! verbose ((^.!! (keyw :verbose)) argv))
  ; XXX TODO call init here
  (for-each (lambda (a) (build-artifact a))
            ((^.!! (keyw :artifacts)) argv)))

)
