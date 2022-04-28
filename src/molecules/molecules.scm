#|

|#

(define atom:charge
  (make-property 'charge
                 'predicate n:exact-integer?
                 'default-value 0))

(define atom:type
  (make-property 'type
                 'predicate symbol?))

(define atom?
  (make-type 'atom (list atom:charge atom:type)))
(set-predicate<=! atom? node?)

(define atom:get-charge
  (property-getter atom:charge atom?))

(define atom:get-type
  (property-getter atom:type atom?))

(define atom:set-charge
  (property-setter atom:charge atom? n:exact-integer?))

(define atom:set-type
  (property-setter atom:type atom? symbol?))

(define atom:create
  (type-instantiator atom?))

;;;; Constants

(define pref-num-bonds
  '((H . 1)
    (C . 4)
    (N . 3)
    (O . 2)))

(define electroneg-order '(N O C H))

;;;; Updater Handler

(define (sort-by-values keys values key:<)
  (let* ((zipped (zip keys values))
         (sorted-zipped
          (sort zipped (lambda (pair1 pair2) (key:< (cadr pair1) (cadr pair2))))))
    (map car sorted-zipped)))

(define (get-max-neighbor-charge mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors mol atom))
         (charges (map atom:get-charge neighbors)))
    (car (sort charges >))))

(define (get-max-charge-neighbor mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors mol atom))
         (sorted-neighbors (sort-by-values neighbors
                                           (map atom:get-charge neighbors)
                                           >)))
    (car sorted-neighbors)))

(define (get-min-neighbor-charge mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors mol atom))
         (charges (map atom:get-charge neighbors)))
    (car (sort charges <))))

(define (get-min-charge-neighbor mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors mol atom))
         (sorted-neighbors (sort-by-values neighbors
                                           (map atom:get-charge neighbors)
                                           <)))
    (car sorted-neighbors)))

(define (resonance-center? graph atom)
  (and (graph? graph)
       (atom? atom)
       (< (get-min-neighbor-charge graph atom) (get-max-neighbor-charge graph atom))))

(define (neighbor-bond-receiver? graph atom)
  (and (graph? graph)
       (atom? atom)
       (< (get-min-neighbor-charge graph atom) 0)
       (> (atom:get-charge atom) 0)))

(define (neighbor-bond-supplier? graph atom)
  (and (graph? graph)
       (atom? atom)
       (> (get-max-neighbor-charge graph atom) 0)
       (< (atom:get-charge atom) 0)))
         
(define-generic-procedure-handler node:update
  (match-args graph? atom?)
  (lambda (mol atom)
    (cond ((resonance-center? mol atom)
           (let* ((sink (get-max-charge-neighbor mol atom))
                  (source (get-min-charge-neighbor mol atom))
                  (old-bond (car (graph:get-edges-from-nodes mol source atom))))
             (atom:set-charge source (+ (atom:get-charge source) 1))
             (atom:set-charge sink (- (atom:get-charge sink) 1))
             (graph:remove-edge mol old-bond)
             (graph:add-edge mol (edge:create 'label (generate-uninterned-symbol "resonance-edge-")
                                              'source atom
                                              'destination sink))))
          ((neighbor-bond-receiver? mol atom)
           (let* ((source (get-min-charge-neighbor mol atom))
                  (sink atom))
             (atom:set-charge source (+ (atom:get-charge source) 1))
             (atom:set-charge sink (- (atom:get-charge sink) 1))
             (graph:add-edge mol (edge:create 'label
                                              (generate-uninterned-symbol "bond-formation-edge-")
                                              'source sink
                                              'destination source))))
           )))
          

(define-generic-procedure-handler tagged-data-representation
  (match-args atom?)
  (lambda (super atom)
    (append (super atom)
            (list (atom:get-charge atom) (atom:get-type atom)))))
