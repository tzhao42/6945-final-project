(load "load")

;; Load old nodes from a file
(load "save-demo.save")

;; Everything is present!

(write-line (node:get-id node-1))
(write-line (node:get-label node-1))
(write-line (node:get-data node-1))
(newline)
(newline)

(write-line (node:get-id node-2))
(write-line (node:get-label node-2))
(write-line (node:get-data node-2))
(newline)
(newline)

(write-line (node:get-id node-3))
(write-line (node:get-label node-3))
(write-line (node:get-data node-3))
(newline)
(newline)

(write-line (edge:get-id edge-4))
(write-line (edge:get-label edge-4))
(write-line (edge:get-data edge-4))
(newline)
(newline)

(write-line (edge:get-id edge-5))
(write-line (edge:get-label edge-5))
(write-line (edge:get-data edge-5))
(newline)
(newline)

(write-line (graph:get-nodes graph-name-placeholder))
(write-line (graph:get-edges graph-name-placeholder))


