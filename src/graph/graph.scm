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

(define (graph:contains-node? node graph)
  (guarantee node? node)
  (guarantee graph? graph)
  (let ((query-node-id (node:id node))
        (existing-node-ids (map node:id (graph:get-nodes graph))))
    (and (memv query-node-id existing-node-ids) #t)))

(define (graph:contains-edge? edge graph)
  (guarantee edge? edge)
  (guarantee graph? graph)
  (let ((query-edge-id (edge:id edge))
        (existing-edge-ids (map edge:id (graph:get-edges graph))))
    (and (memv query-edge-id existing-edge-ids) #t)))

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

(define (graph:add-node node graph)
  (guarantee node? node)
  (guarantee graph? graph)
  (if (graph:contains-node? node graph)
      (error "Graph already contains node. If you want to replace the node, consider graph:replace-node")
      (graph:set-nodes (cons node (graph:get-nodes graph)) graph)))
      
(define (graph:add-edge edge graph)
  (guarantee edge? edge)
  (guarantee graph? graph)
  (if (graph:contains-edge? edge graph)
      (error "Graph already contains edge. If you want to replace the edge, consider graph:replace-node")
      (graph:set-edges (cons edge (graph:get-edges graph)) graph)))
