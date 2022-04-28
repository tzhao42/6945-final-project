#| Nodes

TODO: write some docs
|#

(define (generate-node-id)
  (generate-uninterned-symbol "node-"))

(define node:id
  (make-property 'id
                 'predicate symbol?
                 'default-supplier generate-node-id)) 

(define (node-label? x)
  (or (string? x) (exact-nonnegative-integer? x)))

(define node:label
  (make-property 'label
                 'predicate node-label?))

(define node:data
  (make-property 'data
                 'predicate any-object?
                 'default-value '()))

(define node?
  (make-type 'node (list node:id node:label node:data)))
(set-predicate<=! node? object?)

(define node:get-id
  (property-getter node:id node?))

(define node:get-data
  (property-getter node:data node?))

(define node:get-label
  (property-getter node:label node?))

(define node:set-data!
  (property-setter node:data node? any-object?))

(define node:create
  (type-instantiator node?))

(define (node:copy node)
  (node:create 'id (node:get-id node)
               'label (node:get-label node)
               'data (node:get-data node)))

(define (node:eqv? n1 n2)
  (guarantee node? n1)
  (guarantee node? n2)
  (and (equal? (node:get-id n1) (node:get-id n2))
       (equal? (node:get-label n1) (node:get-label n2))
       (equal? (node:get-data n1) (node:get-data n2))))

(define (node:equal? n1 n2)
  (guarantee node? n1)
  (guarantee node? n2)
  (and (equal? (node:get-label n1) (node:get-label n2))
       (equal? (node:get-data n1) (node:get-data n2))))
  
(define-generic-procedure-handler tagged-data-representation
  (match-args node?)
  (lambda (super node)
    (list (node:get-label node) 'data (node:get-data node))))
