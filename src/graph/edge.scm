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
(register-predicate! edge-label? 'edge-label)

(define edge:label
  (make-property 'label
                 'predicate edge-label?))

(define edge:data
  (make-property 'data
                 'predicate object?
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
  
(define edge:set-data
  (property-setter edge:data edge? object?))

(define edge:set-source
  (property-setter edge:source edge? node?))

(define edge:set-destination
  (property-setter edge:destination edge? node?))

(define (edge:set-nodes edge source destination)
  (edge:set-source edge source)
  (edge:set-destination edge destination))

(define edge:create
  (type-instantiator edge?))

(define (edge:equal? edge1 edge2)
  (equal? (edge:get-label edge1) (edge:get-label edge2)))
