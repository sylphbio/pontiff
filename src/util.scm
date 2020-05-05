(module util *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process)
(import chicken.process-context)
(import chicken.port)
(import chicken.file)
(import chicken.io)

(import tabulae)
(import ix)

(define (die msg . args) (apply printf (cons (<> "pontiff error: " msg "\n") args)) (exit 1))

(define (executable? a) (if (ix:sexp? a)
                            (eq? (ix:ident->tag ((^.! ident) a)) 'pontiff:executable)
                            (error "not ix sexp" a)))

(define (library? a) (if (ix:sexp? a)
                         (eq? (ix:ident->tag ((^.! ident) a)) 'pontiff:library)
                         (error "not ix sexp" a)))

(define (filter-skippable mm)
  (filter* (lambda (m) (not ((^.!! (keyw :skip-compile)) m))) mm))

(define (save-file path str)
  (call-with-output-file path (lambda (p) (write-string str #f p))))

(define (load-file path)
  (and (file-exists? path)
       (file-readable? path)
       (let ((s (call-with-input-file path (lambda (p) (read-string #f p)))))
            (if (eof-object? s) "" s))))

; XXX get rid of this once I implement a robust ix prettyprinter
(define (pp-ix sx)
  (define lead (<> "(" (stringify:ix (cadr sx)) " "))
  (define kvs (map (lambda (k/v) (<> (first* k/v) " " (second* k/v)))
                   (chop (map stringify:ix (cddr sx)) 2)))
  (define pad (<> "\n" (make-string (string-length lead) #\space)))
  (<> lead (string-intersperse kvs pad) ")\n"))

; same as process-run except it invokes execve instead of execvp
; also always shell out to /bin/sh, using the user's shell seems like a recipe for madness
(define (process-create cmd #!optional args vars)
  (let ((env (foldl (lambda (acc e) (alist-update (car e) (cdr e) acc equal?))
                    (get-environment-variables)
                    (if (list? vars) vars '())))
        (pid (process-fork)))
       (cond ((not (= pid 0)) pid)
             (args (process-execute cmd args env))
             (else (process-execute "/bin/sh" `("-c" ,cmd) env)))))

(define (process-join pid)
  (call-with-values (lambda () (process-wait pid))
                    (lambda (pid clean code) (when (not clean) (die "process ~S terminated abnormally with ~S" pid code))
                                             (when (not (= code 0)) (die "process ~S exited with status ~S" pid code))
                                             '())))

)
