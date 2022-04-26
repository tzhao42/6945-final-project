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

(define (graph:contains-node? graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (let ((query-node-id (node:get-id node))
        (existing-node-ids (map node:get-id (graph:get-nodes graph))))
    (and (memv query-node-id existing-node-ids) #t)))

(define (graph:contains-edge? graph edge)
  (guarantee graph? graph)
  (guarantee edge? edge)
  (let ((query-edge-id (edge:get-id edge))
        (existing-edge-ids (map edge:get-id (graph:get-edges graph))))
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
  (property-setter graph:nodes graph? list-of-nodes?))

(define (graph:set-nodes nodes graph)
  (assert (valid-graph-components? nodes (graph:get-edges graph)))
  (%graph:set-nodes graph nodes))

(define %graph:set-edges
  (property-setter graph:edges graph? list-of-edges?))

(define (graph:set-edges graph edges)
  (assert (valid-graph-components? (graph:get-nodes graph) edges))
  (%graph:set-edges graph edges))

(define %graph:create
  (type-instantiator graph?))

(define (graph:create . plist)
  (let* ((g (apply %graph:create plist))
         (nodes (graph:get-nodes g))
         (edges (graph:get-edges g)))
    (assert (valid-graph-components? nodes edges))
    g))

(define (graph:add-node graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (if (graph:contains-node? graph node)
      (error "Graph already contains node. If you want to replace the node, consider graph:replace-node")
      (graph:set-nodes graph (cons node (graph:get-nodes graph)))))
      
(define (graph:add-edge graph edge)
  (guarantee graph? graph)
  (guarantee edge? edge)
  (if (graph:contains-edge? graph edge)
      (error "Graph already contains edge. If you want to replace the edge, consider graph:replace-node")
      (graph:set-edges graph (cons edge (graph:get-edges graph)))))

(define (graph:remove-node graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (if (graph:contains-node? node graph)
      (let ((relevant-edges (filter (lambda (edge) (edge:has-node? node edge))
                                    (graph:get-edges graph))))
        (graph:remove-edges graph relevant-edges)
        (graph:set-nodes graph (remove node (graph:get-nodes))))
      (error "Graph does not contain node")))
      
(define (graph:remove-edge graph edge)
  (guarantee graph? graph)
  (guarantee edge? edge)
  (if (graph:contains-edge? graph edge)
      (graph:remove-edges graph (list edge))
      (error "Graph does not contain edge")))

(define (graph:remove-edges graph edges)
  (guarantee graph? graph)
  (guarantee list-of-edges? edges)
  (let ((new-edges (filter (lambda (edge) (not (memv edge edges)))
                           (graph:get-edges graph))))
    (graph:set-edges graph new-edges)))
