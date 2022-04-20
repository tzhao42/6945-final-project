#| Nodes

TODO: write some docs
|#

(define (node-id? x)
  (or (string? x) (exact-nonnegative-integer? x)))
(register-predicate! node-id? 'node-id)

(define node:id
  (make-property 'id
                 'predicate node-id?
                 'default-value '()))

(define node:data
  (make-property 'data
                 'predicate object?
                 'default-value '()))

(define node?
  (make-type 'node (list node:id node:data)))
(set-predicate<=! node? object?)

(define node:get-data
  (property-getter node:data node?))

(define node:get-id
  (property-getter node:id node?))

(define node:set-data
  (property-setter node:data node? object?))

(define node:create
  (type-instantiator node?))
