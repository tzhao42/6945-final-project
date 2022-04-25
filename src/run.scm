(load "load")
;; (load "~/Dropbox/classes/6.945/6945-final-project/src/load.scm")

(define bob (node:create 'label 'bob))
(node:get-id bob)
(node:get-label bob)
(node:get-data bob)

(define alice (node:create 'label 'alice))

(define ab-edge (edge:create 'label 'ab-edge
                             'source alice
                             'destination bob))

(define ab-graph (graph:create 'nodes (list alice bob)
                               'edges (list ab-edge)))

(define eve (node:create 'label 'eve))

(define ae-edge (edge:create 'label 'ae-edge
                             'source alice
                             'destination eve))

(define wrong-abe-graph (graph:create 'nodes (list alice bob)
                                      'edges (list ab-edge ae-edge)))

(define good-abe-graph (graph:create 'nodes (list alice bob eve)
                                     'edges (list ab-edge ae-edge)))

(define empty-graph (graph:create))

(define no-label-node (node:create))
