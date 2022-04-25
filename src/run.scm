(load "load")
;; (load "~/Dropbox/classes/6.945/6945-final-project/src/load.scm")

(define bob (node:create 'id 'bob))

(write (node:get-id bob))
(newline)

(define alice (node:create 'id 'alice))

(define ab-edge (edge:create 'source alice
                             'destination bob))

(define ab-graph (graph:create 'nodes (list alice bob)
                               'edges (list ab-edge)))

(define eve (node:create 'id 'eve))

(define ae-edge (edge:create 'source alice
                             'destination eve))

(define wrong-abe-graph (graph:create 'nodes (list alice bob)
                                      'edges (list ab-edge ae-edge)))

(define good-abe-graph (graph:create 'nodes (list alice bob eve)
                                     'edges (list ab-edge ae-edge)))
