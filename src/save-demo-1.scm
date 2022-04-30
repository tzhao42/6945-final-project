(load "load")

;; Test cases
(assert (equal? (s:serialize 'a)
                (list 'quote 'a)))
(assert (equal? (s:serialize (string->uninterned-symbol "test-symbol"))
                (list 'string->uninterned-symbol "test-symbol")))
(assert (equal? (s:serialize 0) 0))
(assert (equal? (s:serialize #t) #t))
(assert (equal? (s:serialize "test") "test"))
(assert (equal? (s:serialize (list 1 2 (list 'a 'b)))
                (list 'list 1 2 (list 'list (list 'quote 'a) (list 'quote 'b)))))
(assert (equal? (s:serialize (list 's:lit-flag 'test)) 'test))
(assert (equal? (s:serialize (list 's:expr-flag
                                   (list 's:lit-flag 'define)
                                   (list 's:lit-flag 'node-1)
                                   (list 's:expr-flag (list 's:lit-flag 'create-node)
                                                      (list 's:lit-flag 'node-1))))
                (list 'define 'node-1 (list 'create-node 'node-1))))

;; Create a few nodes
(define alice (node:create 'label 'alice 'data 0))
(define bob (node:create 'label 'bob 'data 0))
(define eve (node:create 'label 'eve
                         'data (list 1 (list 2 3) 4))) ; Nested lists are saveable

;; Save nodes to a file
(wipe-file "save-demo.save") ; Clean the save file first, it might have old stuff in it
(s:save alice "save-demo.save") ; Each successive save appends source code to the file
(s:save bob "save-demo.save")
(s:save eve "save-demo.save")

;; Create an edge
(define ab-edge (edge:create 'label 'ab-edge
                             'source alice
                             'data "love"
                             'destination bob))
(define ae-edge (edge:create 'label 'ae-edge
                             'source alice
                             'data "hate"
                             'destination eve))

;; Save edge to a file
(s:save ab-edge "save-demo.save")
(s:save ae-edge "save-demo.save")

(define good-abe-graph (graph:create 'nodes (list alice bob eve)
                                     'edges (list ab-edge ae-edge)))

;; Save the graph to a file
(s:save good-abe-graph "save-demo.save")
