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

(define-generic-procedure-handler node:update!
  (match-args graph? node?)
  (lambda (graph node)
    (node:set-data! node (+ 1 (node:get-data node)))
    (values))) 

(define debug-output #f)
(define good-abe-graph-2 (graph:copy good-abe-graph))
(pp good-abe-graph)
(pp good-abe-graph-2)
(graph:converge! good-abe-graph)
(graph:equal? good-abe-graph good-abe-graph-2)
(equal*? good-abe-graph good-abe-graph)
(equal*? good-abe-graph good-abe-graph-2)

(define equilib-graph-set (graph:equilibrate! good-abe-graph))
(for-each pp equilib-graph-set)



;;; Allylic cation

(define allylic-cation
  (let ((c1 (atom:create 'type 'C 'charge 0 'label "c1"))
        (c2 (atom:create 'type 'C 'charge 0 'label "c2"))
        (c3 (atom:create 'type 'C 'charge 1 'label "c3")))
    (let ((g
           (graph:create 'nodes (list c1 c2 c3)
                         'edges (list (edge:create 'label "c1-c2-1"
                                                   'source c1
                                                   'destination c2)
                                      (edge:create 'label "c1-c2-2"
                                                   'source c2
                                                   'destination c1)
                                      (edge:create 'label "c2-c3"
                                                   'source c3
                                                   'destination c2)))))
      (for-each (lambda (node) (pp (graph:get-neighbors g node))) (list c1 c2 c3))
      g)))
      
(%graph:single-update! allylic-cation)
(pp allylic-cation)
(graph:converge! allylic-cation)
(pp (graph:equilibrate! allylic-cation))


(define ketene
  (let ((c1 (atom:create 'type 'C 'charge -1 'label "c1"))
        (c2 (atom:create 'type 'C 'charge 0 'label "c2"))
        (c3 (atom:create 'type 'C 'charge 1 'label "c3")))
    (let ((g
           (graph:create 'nodes (list c1 c2 c3)
                         'edges (list (edge:create 'label "c1-c2-1"
                                                   'source c1
                                                   'destination c2)
                                      (edge:create 'label "c1-c2-2"
                                                   'source c2
                                                   'destination c1)
                                      (edge:create 'label "c1-c2-3"
                                                   'source c2
                                                   'destination c1)
                                      (edge:create 'label "c2-c3"
                                                   'source c3
                                                   'destination c2)))))
      (for-each (lambda (node) (pp (graph:get-neighbors g node))) (list c1 c2 c3))
      g)))

(%graph:single-update! ketene)
(pp ketene)


(define ethene
  (let ((c1 (atom:create 'type 'C 'charge -1 'label "c1"))
        (c2 (atom:create 'type 'C 'charge +1 'label "c2")))
    (let ((g
           (graph:create 'nodes (list c1 c2)
                         'edges (list (edge:create 'label "c1-c2"
                                                   'source c1
                                                   'destination c2)))))
      g)))

(%graph:single-update! ethene)
(pp ethene)
