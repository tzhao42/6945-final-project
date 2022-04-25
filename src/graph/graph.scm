#| Graphs

|#

(define (list-of-nodes? lst)
  (and (list? lst) (for-all? lst node?)))

(define graph:nodes
  (make-property 'nodes
                 'predicate list-of-nodes?
                 'default-value '()))

(define (list-of-edges? lst)
  (and (list? lst) (for-all? lst edge?)))

(define graph:edges
  (make-property 'edges
                 'predicate list-of-edges?
                 'default-value '()))

(define graph?
  (make-type 'graph (list graph:nodes graph:edges)))
(set-predicate<=! graph? object?)

(define graph:get-nodes
  (property-getter graph:nodes graph?)) 

(define graph:get-edges
  (property-getter graph:edges graph?))

(define (graph:contains-node node graph)
  (guarantee node? node)
  (guarantee graph? graph)
  (and (memv node (graph:get-nodes graph)) #t))

(define (graph:contains-edge edge graph)
  (guarantee edge? edge)
  (guarantee graph? graph)
  (and (memv edge (graph:get-edges graph)) #t))

(define (valid-graph? graph)
  (let ((nodes (graph:get-nodes graph))
        (edges (graph:get-edges graph)))
    (valid-graph-components? nodes edges)))

(define (valid-graph-components? nodes edges)
  (guarantee list-of-nodes? nodes)
  (guarantee list-of-edges? edges)
  (let* ((edge-sources (map edge:get-source edges))
         (edge-dests (map edge:get-destination edges))
         (edge-nodes (append edge-sources edge-dests)))
    (for-all? edge-nodes (lambda (node) (and (memv node nodes) #t)))))
    
(define %graph:set-nodes
  (property-setter graph:nodes list-of-nodes? graph?))

(define (graph:set-nodes nodes graph)
  (assert (valid-graph-components? nodes (graph:get-edges graph)))
  (%graph:set-nodes nodes graph))

(define %graph:set-edges
  (property-setter graph:edges list-of-edges? graph?))

(define (graph:set-edges edges graph)
  (assert (valid-graph-components? (graph:get-nodes graph) edges))
  (%graph:set-edges edges graph))

(define %graph:create
  (type-instantiator graph?))

(define (graph:create . plist)
  (let* ((g (apply %graph:create plist))
         (nodes (graph:get-nodes g))
         (edges (graph:get-edges g)))
    (assert (valid-graph-components? nodes edges))
    g))
