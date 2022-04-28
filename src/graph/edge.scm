#| Edges

TODO : docs
|#

(define (generate-edge-id)
  (generate-uninterned-symbol "edge-"))

(define edge:id
  (make-property 'id
                 'predicate symbol?
                 'default-supplier generate-edge-id)) 

(define (edge-label? x)
  (or (string? x) (exact-nonnegative-integer? x)))

(define edge:label
  (make-property 'label
                 'predicate edge-label?))

(define edge:data
  (make-property 'data
                 'predicate any-object?
                 'default-value '()))

(define edge:source
  (make-property 'source
                 'predicate node?))

(define edge:destination
  (make-property 'destination
                 'predicate node?))

(define edge?
  (make-type 'edge (list edge:id edge:label edge:data edge:source edge:destination)))
(set-predicate<=! edge? object?)

(define edge:get-id
  (property-getter edge:id edge?))

(define edge:get-label
  (property-getter edge:label edge?))

(define edge:get-data
  (property-getter edge:data edge?))

(define edge:get-source
  (property-getter edge:source edge?))

(define edge:get-destination
  (property-getter edge:destination edge?))

(define (edge:get-nodes edge)
  (guarantee edge? edge)
  (cons (edge:get-source edge) (edge:get-destination edge)))
  
(define edge:set-data!
  (property-setter edge:data edge? any-object?))

(define edge:set-source!
  (property-setter edge:source edge? node?))

(define edge:set-destination!
  (property-setter edge:destination edge? node?))

(define (edge:set-nodes! edge source destination)
  (edge:set-source edge source)
  (edge:set-destination edge destination))

(define edge:create
  (type-instantiator edge?))

(define (edge:copy edge)
  (edge:create 'id (edge:get-id edge)
               'label (edge:get-label edge)
               'data (edge:get-data edge)
               'source (edge:get-source edge)
               'destination (edge:get-destination edge)))

(define (edge:has-node? edge node)
  (guarantee edge? edge)
  (guarantee node? node)
  (or (node:equal? (edge:get-source edge) node)
      (node:equal? (edge:get-destination edge) node)))

(define (edge:eqv? e1 e2)
  (guarantee edge? e1)
  (guarantee edge? e2)
  (and
   (equal? (edge:get-id e1) (edge:get-id e2))
   (equal? (edge:get-label e1) (edge:get-label e2))
   (equal? (edge:get-data e1) (edge:get-data e2))
   (node:equal? (edge:get-source e1) (edge:get-source e2))
   (node:equal? (edge:get-destination e1) (edge:get-destination e2))))

(define (edge:equal? e1 e2)
  (guarantee edge? e1)
  (guarantee edge? e2)
  (and
   (equal? (edge:get-label e1) (edge:get-label e2))
   (equal? (edge:get-data e1) (edge:get-data e2))
   (node:equal? (edge:get-source e1) (edge:get-source e2))
   (node:equal? (edge:get-destination e1) (edge:get-destination e2))))

(define-generic-procedure-handler tagged-data-representation
  (match-args edge?)
  (lambda (super edge)
    (list (edge:get-label edge)
          (node:get-label (edge:get-source edge)) (quote ->)
          (node:get-label (edge:get-destination edge))
          'data (edge:get-data edge))))

