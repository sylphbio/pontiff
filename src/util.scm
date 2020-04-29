(module util *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process)
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

(define (process-join pid)
  (call-with-values (lambda () (process-wait pid))
                    (lambda (pid clean code) (when (not clean) (die "process ~S terminated abnormally with ~S" pid code))
                                             (when (not (= code 0)) (die "process ~S exited with status ~S" pid code))
                                             '())))

)
