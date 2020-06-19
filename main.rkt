#lang racket/base

(provide
 ;; TODO - contracts.  Note that the contract for extenorc field names should be (and/c symbol? symbol-interned?)
 empty-extenor
 ;; TODO - a basic extenor with some good properties -- eg. prop:custom-write
 add-extenorc
 get-extenor-field
 set-extenor-field
 get-extenor-keys
 get-extenor-struct-type-properties
 define-extenorc
 make-extenorc
 make-prop-extenorc
 extenor?
 extenorc?
 ; remove-extenorc
 ; merge-extenors
 )

;; TODO - document the structure of these

;; An extenor has:
;; * The struct-type constructor for its type (for quickly setting fields without
;;   defining a new struct-type with all the properties).
;; * A hash table from extenorc to the extenorc's index in the content vector.
;;   If there are no fields in the extenorc, index is #f.
;; * A content vector, which itself contains:
;;   - For an extenorc with one field, it just has that field's value directly.
;;   - For an extenorc with more than one field, a vector for those fields.
;;   - Nothing for an extenorc with no fields (eg. maybe it has just properties).
(struct extenor (struct-type-constructor extenorc-table content-vector))

(struct extenorc (field-spec-list property-list guard))

(define empty-extenor (extenor (hasheq) (vector-immutable)))

(define (add-extenorc an-extenor an-extenorc . field-vals)
  ;; Check whether the extenorc is already there.
  (when (hash-contains-key? an-extenor an-extenorc)
    (error 'add-extenorc "the extenor already contains the extenorc: ~v" an-extenorc))
  ;; Check whether any visible fields of the extenorc conflict with existing
  ;; visible fields in the extenor.
  (void 'TODO)
  ;; Check whether any properties of the extenorc conflict with existing properties
  ;; on the extenor.
  (void 'TODO)
  ;; If there are new properties, we need to make a new struct-type
  ;; that is a subtype of extenor that has all struct-type-properties
  ;; of all extenorcs.
  (void 'TODO)
  ;; If there are new fields, we need to extend the content-vector.
  (void 'TODO)
  ;; For all extenorcs we need to extend the extenorc table.
  (void 'TODO)
  (error 'TODO-implement-add-extenorc))

(define opaque-flag (gensym))

(define (do-extenor-walk set? an-extenor field-symbol new-value-or-fallback)
  (define result-tentative
    (for/fold ([result opaque-flag])
              ([(extenorc index) (in-hash (extenor-extenorc-table an-extenor))])
      #:break (not (eq? result opaque-flag))
      (if (eq? field-symbol extenorc)
          (if set?
              (vector-set (extenor-content-vector an-extenor)
                          index new-value-or-fallback)
              (vector-ref (extenor-content-vector an-extenor) index))
          (for/fold ([result result])
                    ([field-spec (extenorc-field-spec-list extenorc)]
                     [jndex (in-naturals)])
            #:break (not (eq? result opaque-flag))
            (if (and (eq? (car field-spec) 'visible)
                     (eq? (cdr field-spec) field-symbol))
                (let ([inner (vector-ref (extenor-content-vector an-extenor)
                                         index)])
                  (if (null? (cdr (extenorc-field-spec-list extenorc)))
                      (if set?
                          (vector-set (extenor-content-vector an-extenor)
                                      index new-value-or-fallback)
                          inner)
                      (if set?
                          (vector-set inner jndex new-value-or-fallback)
                          (vector-ref inner jndex))))
                result)))))
  (if (eq? result-tentative opaque-flag)
      (if set?
          (let* ([new-index (vector-length (extenor-content-vector an-extenor))]
                 [new-content-vector (apply vector-immutable
                                            (append (vector->list
                                                     (extenor-content-vector))
                                                    (list new-value-or-fallback)))]
                 [new-table (hash-set (extenor-extenorc-table an-extenor)
                                      field-symbol
                                      new-index)]
                 [st-ctor (extenor-struct-type-constructor an-extenor)])
            (st-ctor st-ctor new-content-vector new-table))
          (if (procedure? new-value-or-fallback)
              (new-value-or-fallback)
              new-value-or-fallback))
      (if set?
          (void)
          result-tentative)))

(define (get-extenor-field an-extenor field-symbol
                           [fallback (λ () (error 'get-extenor-field
                                                  "No value found for key: ~v"
                                                  field-symbol))])
  (do-extenor-walk #f an-extenor field-symbol fallback))

(define (set-extenor-field an-extenor field-symbol new-value)
  (do-extenor-walk #t an-extenor field-symbol fallback))

(define (get-extenor-keys an-extenor)
  (error 'TODO-implement-get-extenor-keys))

(define (get-extenor-struct-type-properties an-extenor)
  (error 'TODO-implement-get-extenor-struct-type-properties))

(define-syntax (define-extenorc stx)
  (define-syntax-class field-spec
    (pattern field-name:id #:attr visibility #''visible)
    (pattern [(~datum hidden) field-name:id] #:attr visibility #''hidden)
    (pattern [(~datum visible) field-name:id] #:attr visibility #''visible))
  (syntax-parse stx
    [(_ name:id (fs:field-spec ...)
        (~or
         (~optional (~seq #:guard guard-expression)
                    #:defaults ([guard-expression #'#f]))
         (~seq #:property prop-expression prop-val-expression))
        ...)
     (with-syntax ([(accessor ...) (map (λ (fn) (format-id fn "~a-~a" #'name fn))
                                        (syntax->list #'(fs.field-name ...)))])
       #'(define-values
           (name accessor ...)
           (make-extenorc #:name 'name
                          #:guard guard-expression
                          #:properties (make-immutable-hash
                                        (~@ (cons prop-expression
                                                  prop-val-expression)
                                            ...))
                          (cons 'fs.visibility 'fs.field-name) ...)))]))

(define (make-extenorc
         #:name [name #f]
         #:guard [guard #f]
         #:properties [properties (hash)]
         . field-name-spec)
  (error 'TODO-implement-make-extenorc))

(define (make-prop-extenorc prop prop-val)
  (error 'TODO-implement-make-prop-extenorc))
