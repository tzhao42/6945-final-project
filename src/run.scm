(load "load")
;; (load "~/Dropbox/classes/6.945/6945-final-project/src/load.scm")

(define bob (node:create 'label 'bob
                         'data 0))
(node:get-id bob)
(node:get-label bob)
(node:get-data bob)

(define alice (node:create 'label 'alice
                           'data 0))

(define ab-edge (edge:create 'label 'ab-edge
                             'source alice
                             'destination bob))

(define ab-graph (graph:create 'nodes (list alice bob)
                               'edges (list ab-edge)))

(define eve (node:create 'label 'eve
                         'data 0))
(graph:contains-node? ab-graph eve)
(graph:add-node! ab-graph eve)

(define ae-edge (edge:create 'label 'ae-edge
                             'source alice
                             'destination eve))

(define wrong-abe-graph (graph:create 'nodes (list alice bob)
                                      'edges (list ab-edge ae-edge)))

(define good-abe-graph (graph:create 'nodes (list alice bob eve)
                                     'edges (list ab-edge ae-edge)))

(define empty-graph (graph:create))

(define no-label-node (node:create))

(graph:get-nodes empty-graph)
(%graph:set-nodes! empty-graph (list alice))
(graph:get-nodes empty-graph)

(pp good-abe-graph)
(graph:get-edges good-abe-graph)
(graph:remove-edge! good-abe-graph ab-edge)
(pp good-abe-graph)

(graph:update! good-abe-graph)

(define (predicate graph node)
  (and (graph? graph) (node? node)))

(define-generic-procedure-handler node:update!
  predicate
  (lambda (graph node)
    (node:set-data! node (+ 1 (node:get-data node)))
    (values))) 

(define good-abe-graph-2 (graph:copy good-abe-graph))
(pp good-abe-graph)
(pp good-abe-graph-2)
(graph:converge! good-abe-graph)
(graph:equal? good-abe-graph good-abe-graph-2)
(equal*? good-abe-graph good-abe-graph)
(equal*? good-abe-graph good-abe-graph-2)

(define equilib-graph-set (graph:equilibrate! good-abe-graph))
(for-each pp equilib-graph-set)
