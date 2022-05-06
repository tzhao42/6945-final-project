(load "load")

;; Load old nodes from a file
(load "save-demo.save")

;; Everything is present!

(begin
  (newline)
  (pp node-1)
  (newline))

(begin
  (newline)
  (pp node-2)
  (newline))

(begin
  (newline)
  (pp node-3)
  (newline))

(begin
  (newline)
  (pp edge-4)
  (newline))

(begin
  (newline)
  (pp edge-5)
  (newline))

(pp graph-name-placeholder)
(write-line (graph:get-nodes graph-name-placeholder))
(write-line (graph:get-edges graph-name-placeholder))


