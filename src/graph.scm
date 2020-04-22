(module graph *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae)

(define (vertexes adjl)
  (map first* adjl))

; adjlist -> (leaves branches)
(define (cutleaves adjl)
  (let ((vv (vertexes adjl)))
       (partition* (lambda (v/e) (null? (intersect* vv (second* v/e)))) adjl)))

)
