;;; Base object type

(define object:name
  (make-property 'name))

(define object:description
  (make-property 'description
                 'default-to-property object:name))

(define object?
  (make-type 'object (list object:name object:description)))

(define get-name
  (property-getter object:name object?))

(define get-description
  (property-getter object:description object?))

(define (find-object-by-name name objects)
  (find (lambda (object)
          (eqv? name (get-name object)))
        objects))

(define-generic-procedure-handler tagged-data-representation
  (match-args object?)
  (lambda (super object)
    (append (super object)
            (list (get-name object)))))

(define-generic-procedure-handler tagged-data-description
  (match-args object?)
  (lambda (object)
    (let ((instance-data (tagged-data-data object)))
      (map (lambda (property)
             (list (property-name property)
                   ((instance-data-binding property
                                           instance-data))))
           (instance-data-properties instance-data)))))
