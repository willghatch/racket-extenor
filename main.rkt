#lang racket/base

(require racket/contract)
(define (isym? x) (and (symbol? x) (symbol-interned? x)))

(provide
 (contract-out
  [extenor? (-> any/c any/c)]
  [empty-extenor extenor?]
  [add-extenorc (->* (extenor? extenorc?) () #:rest (listof any/c) extenor?)]
  [get-extenor-field (->* (extenor? isym?) (any/c) any/c)]
  [set-extenor-field (-> extenor? isym? any/c any/c)]
  [get-extenor-keys (-> extenor? (listof isym?))]
  [get-extenor-struct-type-properties (-> extenor? (listof struct-type-property?))]
  [remove-extenorc (-> extenor? extenorc? extenor?)]
  ; TODO - merge-extenors

  [make-extenorc (->* ()
                      (#:name (or/c not isym?)
                       #:guard (or/c not procedure?)
                       #:properties (hash/c struct-type-property? any/c))
                      #:rest (listof (cons/c (or/c 'hidden 'visible)
                                             isym?))
                      (list/c extenorc?
                              (-> any/c any/c)
                              (listof (-> extenor? any/c))
                              (listof (-> extenor? any/c extenor?))))]
  [extenorc? (-> any/c any/c)]
  [extenorc-name (-> extenorc? any/c)]
  [get-extenorc-struct-type-properties
   (-> extenorc? (listof struct-type-property?))]
  [make-prop-extenorc (-> struct-type-property? any/c extenorc?)]
  )
 define-extenorc
 ;; TODO - a basic extenor with some good properties -- eg. prop:custom-write
 )

(require
 racket/list
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

;; An extenor has:
;; * The struct-type constructor for its type (for quickly setting fields without
;;   defining a new struct-type with all the properties).
;; * A hash table from extenorc to the extenorc's data.
;;   -  If there is one field, the hash table value for the extenorc is that field.
;;   -  If there are multiple fields, the hash table holds a list of the fields.
;; Extenors can be extended with more extenorcs.  If an extenorc adds a new property,
;; the extended extenor must use a new subtype of the base extenor struct that includes
;; the property.  If the extenorc does not add a new property, then the current struct
;; constructor can be re-used.
(struct extenor (struct-constructor extenorc-table))

;; extenorcs have:
;; * A name, which is an interned symbol or #f. (TODO - this may change)
;; * A field spec list, where each field spec is (cons visibility field-name)
;;   where visibility is either 'hidden or 'visible and field-name is an
;;   interned symbol.
;; * A property alist, mapping struct-type-properties to the relevant values.
;; * a guard, which is either #f or a function that accepts one argument for
;;   each field and returns a list of the same number of fields.  It can
;;   either raise an exception to block construction with bad data or it
;;   can transform the data for construction.
(struct extenorc (name field-spec-list property-alist guard))

(define empty-extenor (extenor extenor (hasheq)))

(define (build-extenor-constructor prop-alist)
  (define-values (type constructor predicate accessor mutator)
    (make-struct-type (gensym) struct:extenor 0 0 0 prop-alist))
  constructor)

(define (add-extenorc an-extenor an-extenorc . field-vals)
  ;; Check whether the extenorc is already there.
  (when (hash-has-key? (extenor-extenorc-table an-extenor) an-extenorc)
    (error 'add-extenorc "the extenor already contains the extenorc: ~v" an-extenorc))
  ;; Check whether any visible fields of the extenorc conflict with existing
  ;; visible fields in the extenor.
  (define field-specs (extenorc-field-spec-list an-extenorc))
  (define field-conflict
    (for/or ([field-spec field-specs])
      (and (eq? (car field-spec) 'visible)
           (not (eq? opaque-flag
                     (get-extenor-field an-extenor
                                        (cdr field-spec)
                                        opaque-flag)))
           (cdr field-spec))))
  (when field-conflict
    (error 'add-extenorc "extenor already contains visible field: ~v"
           field-conflict))
  ;; Check whether any properties of the extenorc conflict with existing properties
  ;; on the extenor.
  (define new-props-alist (extenorc-property-alist an-extenorc))
  (define old-props-alist
    (and (not (null? new-props-alist))
         (for/fold ([props '()])
                   ([extenorc (hash-keys
                               (extenor-extenorc-table an-extenor))])
           (append (if (symbol? extenorc)
                       '()
                       (extenorc-property-alist extenorc))
                   props))))
  (define prop-conflict
    (for/or ([prop (map car (extenorc-property-alist an-extenorc))])
      (and (assq prop old-props-alist)
           prop)))
  (when prop-conflict
    (error 'add-extenorc "extenor already contains struct-type-property: ~v"
           prop-conflict))
  ;; If there are new properties, we need to make a new struct-type
  ;; that is a subtype of extenor that has all struct-type-properties
  ;; of all extenorcs.
  (define new-constructor
    (if (null? new-props-alist)
        (extenor-struct-constructor an-extenor)
        (build-extenor-constructor (append new-props-alist old-props-alist))))

  ;; We need to add any new fields, applying guards as necessary.
  (define field-vals-l (length field-vals))
  (define field-specs-l (length field-specs))
  (when (not (eq? field-vals-l
                  field-specs-l))
    (error 'add-extenorc "Improper number of field values.  Expected ~a, received ~a"
           field-specs-l
           field-vals-l))
  (define guarded-field-vals
    (if (extenorc-guard extenorc)
        (apply extenorc-guard field-vals)
        field-vals))
  (when (and (not (eq? field-vals guarded-field-vals))
             (not (eq? field-vals-l (length guarded-field-vals))))
    (error 'add-extenorc
           "Guard procedure returned the wrong number of fields for extenorc: ~v"
           (extenorc-name an-extenorc)))
  (define new-contents
    (cond [(eq? 0 field-vals-l) '()]
          [(eq? 1 field-vals-l) (car guarded-field-vals)]
          [else guarded-field-vals]))
  (define new-table (hash-set (extenor-extenorc-table an-extenor)
                              an-extenorc
                              new-contents))
  (new-constructor new-constructor new-table))

(define (remove-extenorc an-extenor an-extenorc)
  (define extenor-constructor (extenor-struct-constructor an-extenor))
  (define new-table (hash-remove (extenor-extenorc-table an-extenor) an-extenorc))
  (if (null? (extenorc-property-alist an-extenorc))
      (extenor-constructor
       extenor-constructor
       (hash-remove (extenor-extenorc-table an-extenor) an-extenorc))
      (let* ([prop-alist (for/fold ([props '()])
                                   ([extenorc (hash-keys new-table)])
                           (if (symbol? extenorc)
                               props
                               (append (extenorc-property-alist extenorc)
                                       props)))]
             [new-constructor (build-extenor-constructor prop-alist)])
        (new-constructor new-constructor new-table))))


(define (do-extenor-walk/break set? an-extenor field-symbol new-value-or-fallback)
  (define result-tentative
    (for/fold ([result opaque-flag])
              ([(extenorc contents) (in-hash (extenor-extenorc-table an-extenor))])
      #:break (not (eq? result opaque-flag))
      (if (eq? field-symbol extenorc)
          (if set?
              (hash-set (extenor-extenorc-table an-extenor)
                        extenorc new-value-or-fallback)
              contents)
          (let* ([fields (extenorc-field-spec-list extenorc)]
                 [single-field? (list-length-one? fields)])
            (for/fold ([result result])
                      ([field-spec fields]
                       [index (in-naturals)])
              #:break (not (eq? result opaque-flag))
              (if (and (eq? (car field-spec) 'visible)
                       (eq? (cdr field-spec) field-symbol))
                  (if set?
                      (let* ([guard-proc (or (extenorc-guard extenorc)
                                             (λ(x)x))]
                             [new-contents
                              (if single-field?
                                  (guard-proc (list new-value-or-fallback))
                                  (guard-proc
                                   (list-set contents index new-value-or-fallback)))])
                        (hash-set (extenor-extenorc-table an-extenor)
                                  extenorc
                                  (if single-field?
                                      (car new-contents)
                                      new-contents)))
                      (if single-field?
                          contents
                          (list-ref contents index)))
                  result))))))
  (if (eq? result-tentative opaque-flag)
      (if set?
          (let ([new-table (hash-set (extenor-extenorc-table an-extenor)
                                     field-symbol
                                     new-value-or-fallback)]
                [st-ctor (extenor-struct-constructor an-extenor)])
            (st-ctor st-ctor new-table))
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
  (do-extenor-walk/break #f an-extenor field-symbol fallback))

(define (set-extenor-field an-extenor field-symbol new-value)
  (do-extenor-walk/break #t an-extenor field-symbol new-value))

(define (get-extenor-keys an-extenor)
  (for/fold ([result '()])
            ([extenorc (hash-keys (extenor-extenorc-table an-extenor))])
    (append (filter-map (λ (fs) (and (eq? 'visible (car fs))
                                     (cdr fs)))
                        (extenorc-field-spec-list extenorc))
            result)))

(define (get-extenor-struct-type-properties an-extenor)
  (for/fold ([result '()])
            ([extenorc (hash-keys (extenor-extenorc-table an-extenor))])
    (append (map car (extenorc-property-alist extenorc)) result)))

(define (get-extenorc-struct-type-properties an-extenorc)
  (map car (extenorc-property-alist an-extenorc)))

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
     (with-syntax ([(getter ...) (map (λ (fn) (format-id fn "~a-~a" #'name fn))
                                      (syntax->list #'(fs.field-name ...)))])
       (with-syntax ([(setter ...) (map (λ (fn) (format-id fn "set-~a-~a" #'name fn))
                                        (syntax->list #'(fs.field-name ...)))])
         #'(define-values
             (name predicate getter ... setter ...)
             (apply values
                    (flatten
                     (make-extenorc #:name 'name
                                    #:guard guard-expression
                                    #:properties (make-immutable-hash
                                                  (~@ (cons prop-expression
                                                            prop-val-expression)
                                                      ...))
                                    (cons 'fs.visibility 'fs.field-name) ...))))))]))

(define (make-extenorc
         #:name [name #f]
         #:guard [guard #f]
         #:properties [properties (hash)]
         . field-name-spec)
  (define this-extenorc
    (extenorc name
              field-name-spec
              (for/list ([(key val) (in-hash properties)])
                #;(when (not (struct-type-property? key))
                    (error 'make-extenorc "Not a struct-type-property: ~v" key))
                (cons key val))
              guard))
  (define this-extenorc-predicate
    (λ (an-extenor)
      (and (extenor? an-extenor)
           (hash-has-key? (extenor-extenorc-table an-extenor)
                          this-extenorc))))
  (define single-field?
    (list-length-one? field-name-spec))
  (define this-extenorc-getters
    (for/list ([fns field-name-spec]
               [index (in-naturals)])
      (λ (an-extenor)
        (let ([contents
               (hash-ref (extenor-extenorc-table an-extenor)
                         this-extenorc
                         (λ () (error 'extenorc-getter
                                      "extenorc (~v) not found in extenor (~v)"
                                      name
                                      an-extenor)))])
          (if single-field?
              contents
              (list-ref contents index))))))
  (define this-extenorc-setters
    (for/list ([fns field-name-spec]
               [index (in-naturals)])
      (λ (an-extenor new-val)
        (when (not (this-extenorc-predicate an-extenor))
          (error 'extenorc-setter
                 "extenorc (~v) not found in extenor (~v)"
                 name
                 an-extenor))
        (if single-field?
            (hash-set (extenor-extenorc-table an-extenor)
                      this-extenorc
                      new-val)
            (let ([contents
                   (hash-ref (extenor-extenorc-table an-extenor)
                             this-extenorc)])
              (hash-set (extenor-extenorc-table an-extenor)
                        this-extenorc
                        (list-set contents index new-val)))))))
  (list
   this-extenorc
   this-extenorc-predicate
   this-extenorc-getters
   this-extenorc-setters))

(define (make-prop-extenorc prop prop-val)
  (car (make-extenorc #:properties (hash prop prop-val))))

(define (list-length-one? l)
  (and (not (null? l))
       (null? (cdr l))))

(define opaque-flag (gensym))
