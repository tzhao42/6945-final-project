#| Nodes

TODO: write some docs
|#

(define (node-label? x)
  (or (string? x) (exact-nonnegative-integer? x)))
(register-predicate! node-label? 'node-label)

(define node:label
  (make-property 'label
                 'predicate node-label?))

(define node:data
  (make-property 'data
                 'predicate object?
                 'default-value '()))

(define node?
  (make-type 'node (list node:label node:data)))
(set-predicate<=! node? object?)

(define node:get-data
  (property-getter node:data node?))

(define node:get-label
  (property-getter node:label node?))

(define node:set-data
  (property-setter node:data node? object?))

(define node:create
  (type-instantiator node?))

(define (node:equal? node1 node2)
  (equal? (node:get-label node1) (node:get-label node2)))
