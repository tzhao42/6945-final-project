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

(define graph:update-iteration-cap 10)

(define graph:update
  (most-specific-generic-procedure
   'graph:update 2
   (error-generic-procedure-handler 'graph:update)))

(define (%graph:single-update graph)
  (guarantee graph? graph)
  (for-each (lambda (node)
              (node:update graph node))
            (graph:get-nodes graph))
  (for-each (lambda (edge)
              (edge:update graph edge))
            (graph:get-edges graph)))

(define-generic-procedure-handler graph:update
  (match-args graph? n:exact-nonnegative-integer?)
  (lambda (graph iter-left)
    (if (= iter-left 0)
        (begin
          (display "Graph updater has hit max iteration cap")
          'done-capped)
        (let ((graph-copy (graph:copy graph)))
          (%graph:single-update graph)
          (if (not (graph:equal? graph graph-copy))
              (graph:update graph (- iter-left 1))
              'done)))))
              
(define (graph:converge graph)
  (graph:update graph graph:update-iteration-cap))

(define (graph:memory-step graph memory)
  (%graph:single-update graph)
  (lset-adjoin graph:equal? memory (graph:copy graph)))

(define (graph:equilibrate graph)
  (define (equilibrate graph memory iter-left)
    (if (= 0 iter-left)
        memory
        (let ((new-memory (graph:memory-step graph memory)))
          (if (lset= graph:equal? memory new-memory)
              memory
              (equilibrate graph new-memory (- iter-left 1))))))
  (if (equal? 'done-capped (graph:converge graph))
      (equilibrate graph '() graph:update-iteration-cap)
      (list graph)))
