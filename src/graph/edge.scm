#| Edges

TODO : docs
|#

(define (edge-id? x)
  (or (string? x) (exact-nonnegative-integer? x)))
(register-predicate! edge-id? 'edge-id)

(define edge:id
  (make-property 'id
                 'predicate edge-id?
                 'default-value '()))

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
  (make-type 'edge (list edge:source edge:destination edge:data edge:id)))
(set-predicate<=! edge? object?)

(define edge:get-id
  (property-getter edge:id edge?))

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
