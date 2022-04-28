#| Graphs

|#

;;;; Utils (TODO: separate file?)

(define (all-distinct? lst)
  (if (< (length lst) 2)
      #t
      (and (not (member (car lst) (cdr lst)))
           (all-distinct? (cdr lst)))))
;; test cases
(all-distinct? '(1 "a" c)) ; -> #t
(all-distinct? '()) ; -> #t
(all-distinct? '(1)) ; -> #t
(all-distinct? '("a" "a")) ; -> #f
(all-distinct? '(c c)) ; -> #f

;;;; Graph Properties

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

;;;; Graph creation

(define graph?
  (make-type 'graph (list graph:nodes graph:edges)))
(set-predicate<=! graph? object?)

(define %graph:create
  (type-instantiator graph?))

(define (graph:create . plist)
  (let* ((g (apply %graph:create plist))
         (nodes (graph:get-nodes g))
         (edges (graph:get-edges g)))
    (assert (valid-graph-components? nodes edges))
    g))

(define (graph:copy graph)
  (let* ((nodes (graph:get-nodes graph))
         (node-copies (map node:copy nodes))
         (node-copy-alist (map cons nodes node-copies))
         (edge-copies (map edge:copy (graph:get-edges graph))))
    (for-each (lambda (edge)
                (edge:set-source
                 edge
                 (cdr
                  (assv (edge:get-source edge) node-copy-alist)))
                (edge:set-destination
                 edge
                 (cdr
                  (assv (edge:get-destination edge) node-copy-alist))))
              edge-copies)
    (graph:create 'nodes node-copies
                  'edges edge-copies)))

;;;; Graph item retrieval

(define graph:get-nodes
  (property-getter graph:nodes graph?)) 

(define (graph:label->node graph node-label)
  (guarantee graph? graph)
  (guarantee node-label? node-label)
  (find (lambda (node) (equal? node-label (node:get-label node)))
        (graph:get-nodes graph)))

(define (graph:get-outgoing-edges graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (let ((edges (graph:get-edges graph)))
    (filter (lambda (edge)
              (equal? node (edge:get-source edge)))
            (graph:get-edges graph))))

(define (graph:get-incoming-edges graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (let ((edges (graph:get-edges graph)))
    (filter (lambda (edge)
              (equal? node (edge:get-destination edge)))
            (graph:get-edges graph))))

(define (graph:get-node-edges graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (append (graph:get-outgoing-edges graph node) (graph:get-incoming-edges graph node)))

(define (graph:get-neighbors graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (lset-union node:equal?
              (map cdr (map edge:get-nodes (graph:get-outgoing-edges graph node)))
              (map car (map edge:get-nodes (graph:get-incoming-edges graph node)))))
              
(define graph:get-edges
  (property-getter graph:edges graph?))

(define (graph:label->edge graph edge-label)
  (guarantee graph? graph)
  (guarantee edge-label? edge-label)
  (find (lambda (edge) (equal? edge-label (edge:get-label edge)))
        (graph:get-edges graph)))

(define (graph:get-edges-from-nodes graph node1 node2)
  (filter (lambda (edge) (and
                          (edge:has-node? edge node1)
                          (edge:has-node? edge node2)))
          (graph:get-edges graph)))
  
;;;; Graph predicates

(define (graph:contains-node? graph node)
  (guarantee graph? graph)
  (guarantee node? node)
  (let ((query-node-id (node:get-id node))
        (existing-node-ids (map node:get-id (graph:get-nodes graph))))
    (and (memv query-node-id existing-node-ids) #t)))

(define (graph:contains-node-label? graph query-label)
  (guarantee graph? graph)
  (guarantee node-label? query-label)
  (let ((existing-node-labels (map node:get-label (graph:get-nodes graph))))
    (and (memv query-label existing-node-labels) #t)))

(define (graph:contains-edge? graph edge)
  (guarantee graph? graph)
  (guarantee edge? edge)
  (let ((query-edge-id (edge:get-id edge))
        (existing-edge-ids (map edge:get-id (graph:get-edges graph))))
    (and (memv query-edge-id existing-edge-ids) #t)))

(define (graph:contains-edge-label? graph query-label)
  (guarantee graph? graph)
  (guarantee edge-label? query-label)
  (let ((existing-edge-labels (map edge:get-label (graph:get-edges graph))))
    (and (memv query-label existing-edge-labels) #t)))

(define (valid-graph? graph)
  (let ((nodes (graph:get-nodes graph))
        (edges (graph:get-edges graph)))
    (valid-graph-components? nodes edges)))

(define (valid-graph-components? nodes edges)
  (guarantee list-of-nodes? nodes)
  (guarantee list-of-edges? edges)
  (guarantee all-distinct? (map node:get-label nodes))
  (guarantee all-distinct? (map edge:get-label edges))
  (let* ((edge-sources (map edge:get-source edges))
         (edge-dests (map edge:get-destination edges))
         (edge-nodes (append edge-sources edge-dests)))
    (for-all? edge-nodes (lambda (node) (and (memv node nodes) #t)))))
    
(define (graph:equal? g1 g2)
  (and
   (lset= node:equal? (graph:get-nodes g1) (graph:get-nodes g2))
   (lset= edge:equal? (graph:get-edges g1) (graph:get-edges g2))))
     

;;; Graph modifiers

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
  (if (graph:contains-node? graph node)
      (let ((relevant-edges (filter (lambda (edge) (edge:has-node? edge node))
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
