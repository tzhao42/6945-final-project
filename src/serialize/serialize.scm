#|
Code for saving objects.

Idea: We write source code to a save file, which is then loaded to construct objects at load time.
|#




;;;; Misc Helpers

(define (%intern-symbol uninterned-sym)
    (intern (symbol->string uninterned-sym)))




;;;; Primitive functions for interacting with files

(define (wipe-file file-path)
    (let ((file-port (open-output-file file-path)))
        (newline file-port)
        (close-port file-port)))

(define (append-text-to-file file-path text)
    (let ((file-port (open-output-file file-path #t)))
        (write text file-port)
        (close-port file-port)))

(define (append-code-to-file file-path text)
    (let ((file-port (open-output-file file-path #t)))
        (pp text file-port #t)
        (close-port file-port)))

(define (append-newline-to-file file-path)
    (let ((file-port (open-output-file file-path #t)))
        (newline file-port)
        (close-port file-port)))

(define (call-with-output-file-append file-path thunk) ; Useful for saving entire functions?
    (let ((file-port (open-output-file file-path #t)))
        (thunk file-port)
        (close-port file-port)))




;;;; Basic serialization utilities

(define s:serialize
  (simple-generic-procedure
   's:serialize 1
   (error-generic-procedure-handler 's:serialize)))

(define (serialize-interned-symbol interned-sym)
    `(quote ,interned-sym))

(define-generic-procedure-handler s:serialize
  (match-args interned-symbol?)
  serialize-interned-symbol)

(define (serialize-uninterned-symbol uninterned-sym)
    (let ((uninterned-sym-str (symbol->string uninterned-sym)))
    `(string->uninterned-symbol ,uninterned-sym-str)))

(define-generic-procedure-handler s:serialize
  (match-args uninterned-symbol?)
  serialize-uninterned-symbol)

(define (s:self-evaluating? exp)
  (or (number? exp)
      (boolean? exp)
      (string? exp)))

(register-predicate! s:self-evaluating? 's:self-evaluating)

(define (serialize-self-evaluating obj) obj)

(define-generic-procedure-handler s:serialize
  (match-args s:self-evaluating?)
  serialize-self-evaluating)

(define (serialize-list l)
    (let ((serialized-innards (map s:serialize l)))
        (cons 'list serialized-innards)))

(define-generic-procedure-handler s:serialize
  (match-args list?)
  serialize-list)

;;;; Advanced serialization utilities

(define (s:literal? exp)
    (and (list? exp)
         (not (null? exp))
         (symbol? (cadr exp))
         (eq? (car exp) 's:lit-flag)))

(register-predicate! s:literal? 's:literal)

(define (serialize-literal obj) (cadr obj))

(define-generic-procedure-handler s:serialize
  (match-args s:literal?)
  serialize-literal)

(define (s:expression? exp)
    (and (list? exp)
         (not (null? exp))
         (eq? (car exp) 's:expr-flag)))

(register-predicate! s:expression? 's:expression)

(define (serialize-expression obj)
    (let ((body (cdr obj)))
        (map s:serialize body)))

(define-generic-procedure-handler s:serialize
  (match-args s:expression?)
  serialize-expression)




;;;; Saving graph objects

(define s:save
  (simple-generic-procedure
   's:save 2
   (error-generic-procedure-handler 's:save)))

(define (save-node node file-path)
    (append-newline-to-file file-path); Make sure we save on a new line
    (let* ((node-id (node:get-id node))
           (node-label (node:get-label node))
           (node-data (node:get-data node))
           (node-name (%intern-symbol node-id)))
        (let* ((creation-expr (list 's:expr-flag
                                    (list 's:lit-flag 'node:create)
                                    'id node-id
                                    'label node-label
                                    'data node-data))
               (definition-expr (list 's:expr-flag
                                      (list 's:lit-flag 'define)
                                      (list 's:lit-flag node-name)
                                      creation-expr)))
            (append-code-to-file file-path (s:serialize definition-expr))
            (append-newline-to-file file-path))))

(define-generic-procedure-handler s:save
  (match-args node? string?)
  save-node)

(define (save-edge edge file-path)
    (append-newline-to-file file-path); Make sure we save on a new line
    (let* ((edge-id (edge:get-id edge))
           (edge-label (edge:get-label edge))
           (edge-data (edge:get-data edge))
           (edge-source (edge:get-source edge))
           (edge-destination (edge:get-destination edge))
           (edge-name (%intern-symbol edge-id))
           (edge-source-name (%intern-symbol (node:get-id edge-source)))
           (edge-destination-name (%intern-symbol (node:get-id edge-destination))))
        (let* ((creation-expr (list 's:expr-flag
                                    (list 's:lit-flag 'edge:create)
                                    'id edge-id
                                    'label edge-label
                                    'data edge-data
                                    'source (list 's:lit-flag edge-source-name)
                                    'destination (list 's:lit-flag edge-destination-name)))
               (definition-expr (list 's:expr-flag
                                      (list 's:lit-flag 'define)
                                      (list 's:lit-flag edge-name)
                                      creation-expr)))
            ; (write-line definition-expr)
            (append-code-to-file file-path (s:serialize definition-expr))
            (append-newline-to-file file-path))))

(define-generic-procedure-handler s:save
  (match-args edge? string?)
  save-edge)

(define (save-graph graph file-path)
    (append-newline-to-file file-path); Make sure we save on a new line
    (let* ((graph-nodes (graph:get-nodes graph))
           (graph-edges (graph:get-edges graph))
           (graph-nodes-names
                (map (lambda (n) (list 's:lit-flag (%intern-symbol (node:get-id n))))
                     graph-nodes))
           (graph-edges-names
                (map (lambda (e) (list 's:lit-flag (%intern-symbol (edge:get-id e))))
                     graph-edges)))
        (let* ((creation-expr (list 's:expr-flag
                                    (list 's:lit-flag 'graph:create)
                                    'nodes graph-nodes-names
                                    'edges graph-edges-names))
               (definition-expr (list 's:expr-flag
                                      (list 's:lit-flag 'define)
                                      (list 's:lit-flag 'graph-name-placeholder) ;;TODO: fix
                                      creation-expr)))
            ; (write-line definition-expr)
            (append-code-to-file file-path (s:serialize definition-expr))
            (append-newline-to-file file-path))))

(define-generic-procedure-handler s:save
  (match-args graph? string?)
  save-graph)
