#| -*-Scheme-*-

This is a modified version of user-defined-types/adventure-substrate.scm:

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; Properties

(define (make-property name . plist)
  (guarantee n:symbol? name)
  (guarantee property-list? plist)
  (%make-property name
                  (get-predicate-property plist)
                  (get-default-supplier-property plist)))

(define (property-list? object)
  (and (plist? object)
       (<= (count (lambda (keyword)
                    (not (default-object?
                           (plist-value object keyword))))
                  property-default-keywords)
           1)))

(define (get-predicate-property plist)
   (let ((predicate (plist-value plist 'predicate)))
     (if (not (default-object? predicate))
         predicate
         any-object?)))

(define (get-default-supplier-property plist)
  (let ((value (plist-value plist 'default-value))
        (supplier (plist-value plist 'default-supplier))
        (property (plist-value plist 'default-to-property)))
    (cond ((not (default-object? value))
           (lambda (lookup) value))
          ((not (default-object? supplier))
           (lambda (lookup) (supplier)))
          ((not (default-object? property))
           (lambda (lookup) (lookup property)))
          (else #f))))

(define property-default-keywords
  '(default-value default-supplier default-to-property))

(define property-keywords
  `(predicate ,@property-default-keywords))

(define-record-type <property>
    (%make-property name predicate default-supplier)
    property?
  (name property-name)
  (predicate property-predicate)
  (default-supplier property-default-supplier))

(define (property-optional? property)
  (if (property-default-supplier property) #t #f))

(define-record-printer <property>
  (lambda (property)
    (list (property-name property))))

;;;; Types

(define (make-type name properties)
  (guarantee-list-of property? properties)
  (let ((type (simple-abstract-predicate name instance-data?)))
    (%set-type-properties! type properties)
    type))

(define (get-binding property instance)
  (instance-data-binding property (tagged-data-data instance)))


;;; Simplified interface for text -- GJS

(define (get-property-value property object)
  ((get-binding property object)))

(define (set-property-value! property object value)
  ((get-binding property object) value))


(define (type-properties type)
  (append-map %type-properties
              (cons type (all-supertypes type))))

(define (all-supertypes type)
  (filter type? (all-predicate-supersets type)))

(define type?)
(define %type-properties)
(define %set-type-properties!)
(let ((association (make-metadata-association)))
  (set! type? (association 'has?))
  (set! %type-properties (association 'get))
  (set! %set-type-properties! (association 'put!)))

;;;; Instantiation

(define (type-instantiator type)
  (let ((constructor (predicate-constructor type))
        (properties (type-properties type)))
    (lambda plist
      (let ((object
             (constructor (parse-plist plist properties))))
        (set-up! object)
        object))))

;;; TODO: use properties as the keys in the plist.
(define (parse-plist plist properties)
  (define (lookup-value property)
    (let ((value (plist-value plist (property-name property))))
      (if (default-object? value)
          (begin
            (if (not (property-optional? property))
                (error "Missing required property:"
                       (property-name property)
                       plist))
            ((property-default-supplier property) lookup-value))
          value)))
  (make-instance-data
   (map (lambda (property)
          (cons property (lookup-value property)))
        properties)))

(define set-up!
  (chaining-generic-procedure 'set-up! 1
    (constant-generic-procedure-handler #f)))

(define tear-down!
  (chaining-generic-procedure 'tear-down! 1
    (constant-generic-procedure-handler #f)))

;;;; Instance data

(define instance-data?
  (simple-abstract-predicate 'instance-data procedure?))

(define make-instance-data
  (let ((constructor
         (predicate-constructor instance-data?)))
    (lambda (bindings)
      (constructor
       (lambda (#!optional property)
         (if (default-object? property)
             (map car bindings)
             (let ((p (assv property bindings)))
               (if (not p)
                   (error "Unknown property:" property))
               (lambda (#!optional new-value)
                 (if (default-object? new-value)
                     (cdr p)
                     (set-cdr! p new-value))))))))))

(define instance-data-bindings
  (predicate-accessor instance-data?))

(define (instance-data-properties instance-data)
  ((instance-data-bindings instance-data)))

(define (instance-data-binding property instance-data)
  ((instance-data-bindings instance-data) property))

;;;; Methods

(define (property-getter property type)
  (let ((procedure
         (most-specific-generic-procedure
          (symbol 'get- (property-name property))
          1
          #f)))
    (define-generic-procedure-handler procedure
      (match-args type)
      (lambda (object)
        (get-property-value property object)))
    procedure))

(define (property-setter property type value-predicate)
  (let ((procedure
         (most-specific-generic-procedure
          (symbol 'set- (property-name property) '!)
          2
          #f)))
    (define-generic-procedure-handler procedure
      (match-args type value-predicate)
      (lambda (object value)
        (let ((binding (get-binding property object)))
          (%binding-set-prefix property value (binding) object)
          (binding value))))
    procedure))

(define (%binding-set-prefix property new-value old-value object)
  (if debug-output
      (begin
        (send-message! (list ";setting" (possessive object)
                             (property-name property)
                             "to" new-value)
                       debug-output)
        (send-message! (list ";previous value was" old-value)
                       debug-output))))

(define (property-modifier property type value-predicate
                           noun modifier)
  (let ((procedure
         (most-specific-generic-procedure
          (symbol (property-name property) '- noun)
          2
          #f)))
    (define-generic-procedure-handler procedure
      (match-args type value-predicate)
      (lambda (object item)
        (let* ((binding (get-binding property object))
               (old-value (binding))
               (new-value (modifier item old-value)))
          (%binding-set-prefix property new-value old-value
                               object)
          (binding new-value))))
    procedure))

(define (property-adder property type value-predicate)
  (property-modifier property type value-predicate 'adder
                     (lambda (value values)
                       (lset-adjoin eqv? values value))))

(define (property-remover property type value-predicate)
  (property-modifier property type value-predicate 'remover
                     (lambda (value values)
                       (delv value values))))

