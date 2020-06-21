(module graph *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae)
(import ix)

; takes module object to tag and list of import tags
(define (module->adjlist m)
  `(,((^.v (keyw :name)) m)
    ,(map ix:unwrap ((^.v (keyw :local-imports)) m))))

; given a module list, go from tag to module
; this is here because I use it to go back from adjlist tags
; at least that's my story
(define (tag->module modlist tag)
  (find* (lambda (o) (eq? ((^.v (keyw :name)) o) tag)) modlist))

(define (vertexes adjl)
  (map first* adjl))

; adjlist -> (leaves branches)
(define (cutleaves adjl)
  (let ((vv (vertexes adjl)))
       (partition* (lambda (v/e) (null? (intersect* vv (second* v/e)))) adjl)))

; straightforward. actually not used by build because build needs to hash the subgraphs
(define sort-dag
  (letrec ((sort-dag (lambda (adjl acc)
             (let* ((parts (cutleaves adjl))
                    (leaves (first* parts))
                    (branches (second* parts)))
                   (cond ((null? adjl) acc)
                         ((null? leaves) #f) ; graph cycles
                         (else (sort-dag branches (<> acc leaves))))))))
          (lambda (adjl) (sort-dag adjl '()))))

)
