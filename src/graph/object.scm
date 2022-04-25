;;; Base object type

(define object:name
  (make-property 'name
                 'default-value 'object))

(define object?
  (make-type 'object (list object:name)))

(define get-name
  (property-getter object:name object?))

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
