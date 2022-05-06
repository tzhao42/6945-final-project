(load "load")


;;;; Implicit hydrogen test
(define (add-implicit-hydrogen-test)
  (let ((c1 (atom:create 'type 'C 'charge -1 'label "c1")))
    (let ((methyl-anion
           (graph:create 'nodes (list c1)
                         'edges '())))
      (add-implicit-hydrogens! methyl-anion)
      (pp methyl-anion))))
(add-implicit-hydrogen-test)


;;;; Formal Charge computation
;;; Methyl anion
(define methyl-anion
  (let ((c1 (atom:create 'type 'C 'charge 0 'label "c1"))
        (h1 (atom:create 'type 'H 'charge 0 'label "h1"))
        (h2 (atom:create 'type 'H 'charge 0 'label "h2"))
        (h3 (atom:create 'type 'H 'charge 0 'label "h3")))
    (let ((g
           (graph:create 'nodes (list c1 h1 h2 h3)
                         'edges (list (edge:create 'label "c1 lone pair"
                                                   'source c1
                                                   'destination c1)
                                      (edge:create 'label "c1-h1"
                                                   'source c1
                                                   'destination h1)
                                      (edge:create 'label "c1-h3"
                                                   'source c1
                                                   'destination h3)
                                      (edge:create 'label "c1-h2"
                                                   'source c1
                                                   'destination h2)))))
      g)))
(pp methyl-anion)
(%graph:single-update! methyl-anion)

;;; Methyl Cation
(define methyl-cation
  (let ((c1 (atom:create 'type 'C 'charge 0 'label "c1"))
        (h1 (atom:create 'type 'H 'charge 0 'label "h1"))
        (h2 (atom:create 'type 'H 'charge 0 'label "h2"))
        (h3 (atom:create 'type 'H 'charge 0 'label "h3")))
    (let ((g
           (graph:create 'nodes (list c1 h1 h2 h3)
                         'edges (list (edge:create 'label "c1-h1"
                                                   'source c1
                                                   'destination h1)
                                      (edge:create 'label "c1-h3"
                                                   'source c1
                                                   'destination h3)
                                      (edge:create 'label "c1-h2"
                                                   'source c1
                                                   'destination h2)))))
      g)))
(pp methyl-cation)
(%graph:single-update! methyl-cation)


;;;; Bond formation / Molecular Stabilization
;;; ketene but much more unstable
(define ketene-ish
  (let ((c1 (atom:create 'type 'C 'charge -1 'label "c1"))
        (c2 (atom:create 'type 'C 'charge 0 'label "c2"))
        (c3 (atom:create 'type 'C 'charge 1 'label "c3")))
    (let ((g
           (graph:create 'nodes (list c1 c2 c3)
                         'edges (list (edge:create 'label "c1-c2-1"
                                                   'source c1
                                                   'destination c2)
                                      (edge:create 'label "c1-c2-2"
                                                   'source c2
                                                   'destination c1)
                                      (edge:create 'label "c1-c2-3"
                                                   'source c2
                                                   'destination c1)
                                      (edge:create 'label "c2-c3"
                                                   'source c3
                                                   'destination c2)))))
      (pp g)
      (add-implicit-hydrogens! g)
      (pp g)
      g)))
(graph:converge! ketene-ish)
(pp ketene-ish)

;;; Unstable ethene
(define ethene
  (let ((c1 (atom:create 'type 'C 'charge 0 'label "c1"))
        (h1 (atom:create 'type 'H 'charge 0 'label "h1"))
        (h2 (atom:create 'type 'H 'charge 0 'label "h2"))
        (h3 (atom:create 'type 'H 'charge 0 'label "h3"))
        (h4 (atom:create 'type 'H 'charge 0 'label "h4"))
        (c2 (atom:create 'type 'C 'charge 0 'label "c2")))
    (let ((g
           (graph:create 'nodes (list c1 c2 h1 h2 h3 h4)
                         'edges (list (edge:create 'label "c1-c2"
                                                   'source c1
                                                   'destination c2)
                                      (edge:create 'label "c1 lone pair"
                                                   'source c1
                                                   'destination c1)
                                      (edge:create 'label "c1-h1"
                                                   'source c1
                                                   'destination h1)
                                      (edge:create 'label "c1-h2"
                                                   'source c1
                                                   'destination h2)
                                      (edge:create 'label "c2-h3"
                                                   'source c2
                                                   'destination h3)
                                      (edge:create 'label "c2-h4"
                                                   'source c2
                                                   'destination h4)))))
      (pp g)
      g)))
(%graph:single-update! ethene)
(pp ethene)

;;;; Same unstable ethene, but specified with fewer inputs
(define ethene2
  (let ((c1 (atom:create 'type 'C 'charge -1 'label "c1"))
        (c2 (atom:create 'type 'C 'charge 1 'label "c2")))
    (let ((g
           (graph:create 'nodes (list c1 c2)
                         'edges (list (edge:create 'label "c1-c2"
                                                   'source c1
                                                   'destination c2)))))
    (add-implicit-hydrogens! g)
    g)))
(pp ethene2)
(%graph:single-update! ethene2)


;;;; Resonance Examples
;;; Allylic cation
(define allylic-cation
  (let ((c1 (atom:create 'type 'C 'charge 0 'label "c1"))
        (c2 (atom:create 'type 'C 'charge 0 'label "c2"))
        (c3 (atom:create 'type 'C 'charge 1 'label "c3")))
    (let ((g
           (graph:create 'nodes (list c1 c2 c3)
                         'edges (list (edge:create 'label "c1-c2-1"
                                                   'source c1
                                                   'destination c2)
                                      (edge:create 'label "c1-c2-2"
                                                   'source c2
                                                   'destination c1)
                                      (edge:create 'label "c2-c3"
                                                   'source c3
                                                   'destination c2)))))
      (pp g)
      (add-implicit-hydrogens! g)
      (pp g)
      g)))
(%graph:single-update! allylic-cation)
(pp allylic-cation)

;;; Phenyl cation
(define benzene
  (let ((c1 (atom:create 'type 'C 'charge 0 'label "c1"))
        (c2 (atom:create 'type 'C 'charge 0 'label "c2"))
        (c3 (atom:create 'type 'C 'charge 0 'label "c3"))
        (c4 (atom:create 'type 'C 'charge 0 'label "c4"))
        (c5 (atom:create 'type 'C 'charge 1 'label "c5"))
        (c6 (atom:create 'type 'C 'charge 0 'label "c6")))
    (let ((g
           (graph:create 'nodes (list c1 c2 c3 c4 c5 c6)
                         'edges (list (edge:create 'label "c1-c2-1"
                                                   'source c1
                                                   'destination c2)
                                      (edge:create 'label "c1-c2-2"
                                                   'source c1
                                                   'destination c2)
                                      (edge:create 'label "c2-c3"
                                                   'source c2
                                                   'destination c3)
                                      (edge:create 'label "c3-c4-1"
                                                   'source c3
                                                   'destination c4)
                                      (edge:create 'label "c3-c4-2"
                                                   'source c3
                                                   'destination c4)
                                      (edge:create 'label "c4-c5"
                                                   'source c4
                                                   'destination c5)
                                      (edge:create 'label "c5-c6"
                                                   'source c5
                                                   'destination c6)
                                      (edge:create 'label "c6-c1"
                                                   'source c6
                                                   'destination c1)
                                      ))))
      (pp g)
      (add-implicit-hydrogens! g)
      (pp g)
      g)))
(%graph:single-update! benzene)
(pp benzene)
