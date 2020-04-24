(module util *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.io)

(import tabulae)
(import ix)

(define (die msg . args) (apply printf (cons (<> "pontiff error: " msg "\n") args)) (exit 1))

(define (executable? a) (and (ix:sexp? a)
                             (eq? (ix:ident->tag ((^.! ident) a)) 'pontiff:executable)))

(define (library? a) (and (ix:sexp? a)
                          (eq? (ix:ident->tag ((^.! ident) a)) 'pontiff:library)))

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

)
