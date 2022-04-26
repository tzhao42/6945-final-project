#| Graph updaters

|#

;;;; Base node updater

(define node:update
  (most-specific-generic-procedure
   'node:update 2
   (error-generic-procedure-handler 'node:update)))

(define-generic-procedure-handler node:update
  (match-args graph? node?)
  (lambda (graph node)
    (values))) ; return nothing

;;; Base edge updater

(define edge:update
  (most-specific-generic-procedure
   'node:update 2
   (error-generic-procedure-handler 'edge:update)))

(define-generic-procedure-handler edge:update
  (match-args graph? edge?)
  (lambda (graph edge)
    (values))) ; return nothing

;;;; Base graph updater

;; TODO: longer doc
#|
the gist is that graph:update will create a new graph per update iteration
and that node:update and edge:update should only mutate that graph
and the process is repeated until convergence
TODO: maybe worth setting an iteration cap? |#
(define graph:update
  (most-specific-generic-procedure
   'graph:update 1
   (error-generic-procedure-handler 'graph:update)))

(define-generic-procedure-handler graph:update
  (match-args graph?)
  (lambda (graph)
    (let ((graph-copy (graph:copy graph)))
      (for-each (lambda (node)
                  (node:update graph node))
                (graph:get-nodes graph))
      (for-each (lambda (edge)
                  (edge:update graph edge))
                (graph:get-edges graph))
      (if (not (graph:equal? graph graph-copy))
          (graph:update graph)
          'done)))
              
