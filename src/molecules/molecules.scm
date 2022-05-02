#| Molecule parts

|#

;;; atom type

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

(define atom:set-charge!
  (property-setter atom:charge atom? n:exact-integer?))

(define atom:set-type!
  (property-setter atom:type atom? symbol?))

(define atom:create
  (type-instantiator atom?))

;;;; Aliases

(define mol:create graph:create)

(define bond:create edge:create)

;;;; Constants

(define electroneg-order '(O N C H))

(define neg-charge-pref-order electroneg-order) ; worst to best

(define pos-charge-pref-order '(C N O H)) ; worst to best

;;;; Updater Handler

(define (sort-by-values keys values key:<)
  (let* ((zipped (zip keys values))
         (sorted-zipped
          (sort zipped (lambda (pair1 pair2) (key:< (cadr pair1) (cadr pair2))))))
    (map car sorted-zipped)))

(define (mol:get-max-neighbor-charge mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors-strict mol atom))
         (charges (map atom:get-charge neighbors)))
    (if (= 0 (length charges))
        #f
        (car (sort charges >)))))

(define (mol:get-max-charge-neighbors mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors-strict mol atom))
         (max-charge (mol:get-max-neighbor-charge mol atom))
         (filtered-neighbors (filter (lambda (neighbor)
                                       (= max-charge (atom:get-charge neighbor)))
                                     neighbors))
         (sorted-neighbors (sort-by-values
                            filtered-neighbors
                            (map (lambda (type)
                                   (list-index (lambda (t)
                                                 (equal? t type))
                                               pos-charge-pref-order))
                                 (map atom:get-type neighbors))
                            <)))
    (if (= 0 (length sorted-neighbors))
        #f
        sorted-neighbors)))

(define (mol:get-max-charge-neighbor mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let ((neighbors (mol:get-max-charge-neighbors mol atom)))
    (if neighbors
        (car neighbors)
        neighbors)))
        
(define (mol:get-min-neighbor-charge mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors-strict mol atom))
         (charges (map atom:get-charge neighbors)))
    (if (= 0 (length charges))
        #f
        (car (sort charges <)))))

(define (mol:get-min-charge-neighbors mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let* ((neighbors (graph:get-neighbors-strict mol atom))
         (min-charge (mol:get-min-neighbor-charge mol atom))
         (filtered-neighbors (filter (lambda (neighbor)
                                       (= min-charge (atom:get-charge neighbor)))
                                     neighbors))
         (sorted-neighbors (sort-by-values
                            filtered-neighbors
                            (map (lambda (type)
                                   (list-index (lambda (t)
                                                 (equal? t type))
                                               neg-charge-pref-order))
                                 (map atom:get-type neighbors))
                            <)))
    (if (= 0 (length sorted-neighbors))
        #f
        sorted-neighbors)))

(define (mol:get-min-charge-neighbor mol atom)
  (guarantee graph? mol)
  (guarantee atom? atom)
  (let ((neighbors (mol:get-min-charge-neighbors mol atom)))
    (if neighbors
        (car neighbors)
        neighbors)))

(define (all-pairs lst1 lst2)
  (apply append
         (map (lambda (elem1)
                (map (lambda (elem2)
                       (cons elem1 elem2))
                     lst2))
                lst1)))

(define (all-pairs-no-h lst1 lst2)
  (let ((no-h-lst1 (filter (lambda (atom) (not (equal? 'H (atom:get-type atom)))) lst1))
        (no-h-lst2 (filter (lambda (atom) (not (equal? 'H (atom:get-type atom)))) lst2)))
    (apply append
           (map (lambda (elem1)
                  (map (lambda (elem2)
                         (cons elem1 elem2))
                       no-h-lst2))
                no-h-lst1))))

(define (valid-resonance-set? mol source center sink)
  (and (< (atom:get-charge source) (atom:get-charge sink))
       (< 1 (length (graph:get-edges-from-nodes mol source center)))))

(define (resonance-center? mol-atom-pair)
  (and (pair? mol-atom-pair)
       (let ((mol (car mol-atom-pair))
             (atom (cdr mol-atom-pair)))
         (and (graph? mol)
              (atom? atom)
              (not (= 0 (length (graph:get-neighbors mol atom))))
              (let ((sources (mol:get-min-charge-neighbors mol atom))
                    (sinks (mol:get-max-charge-neighbors mol atom)))
                (any
                 (lambda (pair)
                   (let ((source (car pair))
                         (sink (cdr pair)))
                     (valid-resonance-set? mol source atom sink)))
                   (all-pairs-no-h sources sinks))
                 )))))
(register-predicate! resonance-center? 'resonance-center)

(define (neighbor-bond-receiver? mol-atom-pair)
  (and (pair? mol-atom-pair)
       (let ((mol (car mol-atom-pair))
             (atom (cdr mol-atom-pair)))
         (and (graph? mol)
              (atom? atom)
              (not (= 0 (length (graph:get-neighbors mol atom))))
              (< (mol:get-min-neighbor-charge mol atom) 0)
              (> (atom:get-charge atom) 0)))))
(register-predicate! neighbor-bond-receiver? 'neighbor-bond-receiver)

(define (neighbor-bond-supplier? mol-atom-pair)
  (and (pair? mol-atom-pair)
       (let ((mol (car mol-atom-pair))
             (atom (cdr mol-atom-pair)))
         (and (graph? mol)
              (atom? atom)
              (not (= 0 (length (graph:get-neighbors mol atom))))
              (> (mol:get-max-neighbor-charge mol atom) 0)
              (< (atom:get-charge atom) 0)))))
(register-predicate! neighbor-bond-supplier? 'neighbor-bond-supplier)
         
(define mol:dispatch-update!
  (simple-generic-procedure
   'mol:dispatch-update! 1
   (lambda (mol-atom-pair)
     (values))))

(define-generic-procedure-handler mol:dispatch-update!
  (match-args resonance-center?)
  (lambda (mol-atom-pair)
    (let ((mol (car mol-atom-pair))
          (atom (cdr mol-atom-pair)))
      (let* ((sinks (mol:get-max-charge-neighbors mol atom))
             (sources (mol:get-min-charge-neighbors mol atom))
             (source-sink-pair (find
                                (lambda (pair)
                                  (let ((source (car pair))
                                        (sink (cdr pair)))
                                    (valid-resonance-set? mol source atom sink)))
                                (all-pairs-no-h sources sinks)))
             (source (car source-sink-pair))
             (sink (cdr source-sink-pair))
             (old-bond (car (graph:get-edges-from-nodes mol source atom))))
        #|
        (write-line source)
        (write-line sink)
        |#
        (atom:set-charge! source (+ (atom:get-charge source) 1))
        (atom:set-charge! sink (- (atom:get-charge sink) 1))
        (graph:remove-edge! mol old-bond)
        (graph:add-edge! mol (edge:create 'label (generate-uninterned-symbol "resonance-edge-")
                                          'source atom
                                          'destination sink))))))

(define-generic-procedure-handler mol:dispatch-update!
  (match-args neighbor-bond-receiver?)
  (lambda (mol-atom-pair)
    (let ((mol (car mol-atom-pair))
          (atom (cdr mol-atom-pair)))
      (let* ((source (mol:get-min-charge-neighbor mol atom))
             (sink atom))
        (atom:set-charge! source (+ (atom:get-charge source) 1))
        
        (atom:set-charge! sink (- (atom:get-charge sink) 1))
        (graph:remove-edge! mol (car (graph:get-self-edges mol source))) ; remove first lone pair
        (graph:add-edge! mol (edge:create 'label
                                          (generate-uninterned-symbol "bond-formation-edge-")
                                          'source sink
                                          'destination source))))))

(define-generic-procedure-handler mol:dispatch-update!
  (match-args neighbor-bond-supplier?)
  (lambda (mol-atom-pair)
    (let ((mol (car mol-atom-pair))
          (atom (cdr mol-atom-pair)))
      (let* ((sink (mol:get-max-charge-neighbor mol atom))
             (source atom))
        (atom:set-charge! source (+ (atom:get-charge source) 1))
        (atom:set-charge! sink (- (atom:get-charge sink) 1))
        (graph:remove-edge! mol (car (graph:get-self-edges mol source))) ; remove first lone pair
        (graph:add-edge! mol (edge:create 'label
                                          (generate-uninterned-symbol "bond-formation-edge-")
                                          'source sink
                                          'destination source))))))

;;;; Formal Charge computation

(define pref-num-bonds-alist
  '((H . 1)
    (C . 4)
    (N . 3)
    (O . 2)))

(define (atom:get-pref-num-bonds atom)
  (assv (atom:get-type atom) pref-num-bonds-alist))

(define (formal-charge graph atom)
  (let ((pref-bond-num (atom:get-pref-num-bonds atom)))
    (if pref-bond-num
        (- (cdr pref-bond-num)
           (length (graph:get-node-edges graph atom))
           (length (graph:get-self-edges graph atom))) ; each lone pair electron counts as -1
        (error "unable to compute formal charge of" atom " in " graph))))

(define (compute-formal-charge! graph atom)
  (let ((charge (formal-charge graph atom)))
    (atom:set-charge! atom charge)))

(define-generic-procedure-handler node:update!
  (match-args graph? atom?)
  (lambda (mol atom)
    (compute-formal-charge! mol atom)
    (mol:dispatch-update! (cons mol atom))))
          
;;;; Add implicit hydrogens / lone pairs

(define (add-implicit-hydrogens! mol)
  (define (add-lone-pair! mol atom idx)
    (let ((label (node:get-label atom)))
      (graph:add-edge! mol (edge:create 'label (string-append label " lone-pair " (string idx))
                                        'source atom
                                        'destination atom))))
  (define (add-hydrogen! mol atom idx)
    (let* ((label (node:get-label atom))
           (h (atom:create 'label (string-append "[" label "]" "h" (string idx))
                           'type 'H
                           'charge 0)))
      (graph:add-node! mol h)
      (graph:add-edge! mol (edge:create 'label (string-append
                                                (node:get-label atom)
                                                "-"
                                                (node:get-label h))
                                        'source atom
                                        'destination h))))
  (for-each
   (lambda (atom)
     (let ((charge (atom:get-charge atom)))
       (if (< charge 0)
           (for-each (lambda (idx)
                       (add-lone-pair! mol atom idx))
                     (iota (negate charge))))))
   (graph:get-nodes mol))
  (for-each
   (lambda (atom)
     (let ((charge (atom:get-charge atom))
           (num-bonds (length (graph:get-node-edges mol atom)))
           (pref-bond-num (cdr (atom:get-pref-num-bonds atom))))
       (let ((num-bonds-to-add
              (if (> charge 0)
                  (- pref-bond-num num-bonds charge)
                  (- pref-bond-num num-bonds)))) ; neg charge already accounted for
       (if (< 0 num-bonds-to-add)
           (for-each (lambda (idx)
                       (add-hydrogen! mol atom idx))
                     (iota num-bonds-to-add))))))
   (graph:get-nodes mol)))
                                          

;;;; Pretty printing

(define-generic-procedure-handler tagged-data-representation
  (match-args atom?)
  (lambda (super atom)
    (append (super atom)
            (list (atom:get-charge atom) (atom:get-type atom)))))
