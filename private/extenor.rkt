#lang racket/base

(require racket/contract)
(define (isym? x) (and (symbol? x) (symbol-interned? x)))

(provide
 (contract-out
  [extenor? (-> any/c any/c)]
  [empty-extenor extenor?]
  [add-extenorcl (->* (extenor? (or/c extenorcl? isym?))
                      ()
                      #:rest (listof any/c)
                      extenor?)]
  [get-extenor-field (->* (extenor? isym?) (any/c) any/c)]
  [set-extenor-field (-> extenor? isym? any/c any/c)]
  [get-extenor-keys (-> extenor? (listof isym?))]
  [get-extenor-struct-type-properties (-> extenor? (listof struct-type-property?))]
  [remove-extenorcl (-> extenor? extenorcl? extenor?)]
  ; TODO - merge-extenors
  ; TODO - remove-extenorcl-with-key
  ; TODO - remove-extenorcl-with-property

  [make-extenorcl (->* ()
                       (#:name (or/c not isym?)
                        #:guard (or/c not procedure?)
                        #:properties (hash/c struct-type-property? any/c))
                       #:rest (listof (cons/c (or/c 'hidden 'visible)
                                              isym?))
                       (list/c extenorcl?
                               (-> any/c any/c)
                               (listof (-> extenor? any/c))
                               (listof (-> extenor? any/c extenor?))))]
  [rename extenorcl?* extenorcl? (-> any/c any/c)]
  [rename extenorcl-name* extenorcl-name (-> extenorcl? any/c)]
  [get-extenorcl-struct-type-properties (-> extenorcl? (listof struct-type-property?))]
  [make-prop-extenorcl (-> struct-type-property? any/c extenorcl?)]
  )
 define-extenorcl
 ;; TODO - a basic extenor with some good properties -- eg. prop:custom-write
 )

(module+ for-private
  (provide
   extenor-extenorcl-table
   extenorcl-field-spec-list*
   opaque-flag
   single-field-extenorcl?
   ))

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
;; * A hash table from extenorcl to the extenorcl's data.
;;   -  If there is one field, the hash table value for the extenorcl is that field.
;;   -  If there are multiple fields, the hash table holds a list of the fields.
;; Extenors can be extended with more extenorcls.  If an extenorcl adds a new property,
;; the extended extenor must use a new subtype of the base extenor struct that includes
;; the property.  If the extenorcl does not add a new property, then the current struct
;; constructor can be re-used.
(struct extenor (struct-constructor extenorcl-table))

;; extenorcls have:
;; * A name, which is an interned symbol or #f. (TODO - this may change)
;; * A field spec list, where each field spec is (cons visibility field-name)
;;   where visibility is either 'hidden or 'visible and field-name is an
;;   interned symbol.
;; * A property alist, mapping struct-type-properties to the relevant values.
;; * a guard, which is either #f or a function that accepts one argument for
;;   each field and returns a list of the same number of fields.  It can
;;   either raise an exception to block construction with bad data or it
;;   can transform the data for construction.
(struct extenorcl (name field-spec-list property-alist guard))

(define (extenorcl?* ec)
  (or (symbol? ec) (extenorcl? ec)))
(define (extenorcl-name* ec)
  (if (symbol? ec)
      ec
      (extenorcl-name ec)))
(define (extenorcl-field-spec-list* ec)
  (if (symbol? ec)
      (list (cons 'visible ec))
      (extenorcl-field-spec-list ec)))
(define (extenorcl-property-alist* ec)
  (if (symbol? ec)
      '()
      (extenorcl-property-alist ec)))
(define (extenorcl-guard* ec)
  (if (symbol? ec)
      #f
      (extenorcl-guard ec)))

(define empty-extenor (extenor extenor (hasheq)))

(define (build-extenor-constructor prop-alist)
  (define-values (type constructor predicate accessor mutator)
    (make-struct-type (gensym) struct:extenor 0 0 0 prop-alist))
  constructor)

(define (add-extenorcl an-extenor an-extenorcl . field-vals)
  ;; Check whether the extenorcl is already there.
  (when (hash-has-key? (extenor-extenorcl-table an-extenor) an-extenorcl)
    (error 'add-extenorcl "the extenor already contains the extenorcl: ~v" an-extenorcl))
  ;; Check whether any visible fields of the extenorcl conflict with existing
  ;; visible fields in the extenor.
  (define field-specs (if (symbol? an-extenorcl)
                          (list (cons 'visible an-extenorcl))
                          (extenorcl-field-spec-list an-extenorcl)))
  (define field-conflict
    (for/or ([field-spec field-specs])
      (and (eq? (car field-spec) 'visible)
           (not (eq? opaque-flag
                     (get-extenor-field an-extenor
                                        (cdr field-spec)
                                        opaque-flag)))
           (cdr field-spec))))
  (when field-conflict
    (error 'add-extenorcl "extenor already contains visible field: ~v"
           field-conflict))
  ;; Check whether any properties of the extenorcl conflict with existing properties
  ;; on the extenor.
  (define new-props-alist (extenorcl-property-alist* an-extenorcl))
  (define old-props-alist
    (and (not (null? new-props-alist))
         (for/fold ([props '()])
                   ([extenorcl (hash-keys
                                (extenor-extenorcl-table an-extenor))])
           (append (extenorcl-property-alist* extenorcl)
                   props))))
  (define prop-conflict
    (for/or ([prop (map car new-props-alist)])
      (and (assq prop old-props-alist)
           prop)))
  (when prop-conflict
    (error 'add-extenorcl "extenor already contains struct-type-property: ~v"
           prop-conflict))
  ;; If there are new properties, we need to make a new struct-type
  ;; that is a subtype of extenor that has all struct-type-properties
  ;; of all extenorcls.
  (define new-constructor
    (if (null? new-props-alist)
        (extenor-struct-constructor an-extenor)
        (build-extenor-constructor (append new-props-alist old-props-alist))))

  ;; We need to add any new fields, applying guards as necessary.
  (define field-vals-l (length field-vals))
  (define field-specs-l (length field-specs))
  (when (not (eq? field-vals-l
                  field-specs-l))
    (error 'add-extenorcl "Improper number of field values.  Expected ~a, received ~a"
           field-specs-l
           field-vals-l))
  (define guarded-field-vals
    (if (extenorcl-guard* an-extenorcl)
        (apply (extenorcl-guard* an-extenorcl) field-vals)
        field-vals))
  (when (and (not (eq? field-vals guarded-field-vals))
             (not (eq? field-vals-l (length guarded-field-vals))))
    (error 'add-extenorcl
           "Guard procedure returned the wrong number of fields for extenorcl: ~v"
           (extenorcl-name* an-extenorcl)))
  (define new-contents
    (cond [(eq? 0 field-vals-l) '()]
          [(eq? 1 field-vals-l) (car guarded-field-vals)]
          [else guarded-field-vals]))
  (define new-table (hash-set (extenor-extenorcl-table an-extenor)
                              an-extenorcl
                              new-contents))
  (new-constructor new-constructor new-table))

(define (remove-extenorcl an-extenor an-extenorcl)
  (define extenor-constructor (extenor-struct-constructor an-extenor))
  (define new-table (hash-remove (extenor-extenorcl-table an-extenor) an-extenorcl))
  (if (null? (extenorcl-property-alist* an-extenorcl))
      (extenor-constructor
       extenor-constructor
       (hash-remove (extenor-extenorcl-table an-extenor) an-extenorcl))
      (let* ([prop-alist (for/fold ([props '()])
                                   ([extenorcl (hash-keys new-table)])
                           (if (symbol? extenorcl)
                               props
                               (append (extenorcl-property-alist* extenorcl)
                                       props)))]
             [new-constructor (build-extenor-constructor prop-alist)])
        (new-constructor new-constructor new-table))))


(define (do-extenor-walk/break set? an-extenor field-symbol new-value-or-fallback)
  (define result-tentative
    (for/fold ([result opaque-flag])
              ([(extenorcl contents) (in-hash (extenor-extenorcl-table an-extenor))])
      #:break (not (eq? result opaque-flag))
      (if (eq? field-symbol extenorcl)
          (if set?
              (hash-set (extenor-extenorcl-table an-extenor)
                        extenorcl new-value-or-fallback)
              contents)
          (let* ([fields (extenorcl-field-spec-list* extenorcl)]
                 [single-field? (list-length-one? fields)])
            (for/fold ([result result])
                      ([field-spec fields]
                       [index (in-naturals)])
              #:break (not (eq? result opaque-flag))
              (if (and (eq? (car field-spec) 'visible)
                       (eq? (cdr field-spec) field-symbol))
                  (if set?
                      (let* ([guard-proc (or (extenorcl-guard* extenorcl)
                                             (λ(x)x))]
                             [new-contents
                              (if single-field?
                                  (guard-proc (list new-value-or-fallback))
                                  (guard-proc
                                   (list-set contents index new-value-or-fallback)))])
                        (hash-set (extenor-extenorcl-table an-extenor)
                                  extenorcl
                                  (if single-field?
                                      (car new-contents)
                                      new-contents)))
                      (if single-field?
                          contents
                          (list-ref contents index)))
                  result))))))
  (if (eq? result-tentative opaque-flag)
      (if set?
          (let ([new-table (hash-set (extenor-extenorcl-table an-extenor)
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
            ([extenorcl (hash-keys (extenor-extenorcl-table an-extenor))])
    (append (filter-map (λ (fs) (and (eq? 'visible (car fs))
                                     (cdr fs)))
                        (extenorcl-field-spec-list* extenorcl))
            result)))

(define (get-extenor-struct-type-properties an-extenor)
  (for/fold ([result '()])
            ([extenorcl (hash-keys (extenor-extenorcl-table an-extenor))])
    (append (map car (extenorcl-property-alist* extenorcl)) result)))

(define (get-extenorcl-struct-type-properties an-extenorcl)
  (map car (extenorcl-property-alist* an-extenorcl)))

(define-syntax (define-extenorcl stx)
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
                     (make-extenorcl #:name 'name
                                     #:guard guard-expression
                                     #:properties (make-immutable-hash
                                                   (list
                                                    (~@ (cons prop-expression
                                                              prop-val-expression)
                                                        ...)))
                                     (cons fs.visibility 'fs.field-name) ...))))))]))

(define (make-extenorcl
         #:name [name #f]
         #:guard [guard #f]
         #:properties [properties (hash)]
         . field-name-spec)
  (define this-extenorcl
    (extenorcl name
               field-name-spec
               (for/list ([(key val) (in-hash properties)])
                 #;(when (not (struct-type-property? key))
                     (error 'make-extenorcl "Not a struct-type-property: ~v" key))
                 (cons key val))
               guard))
  (define this-extenorcl-predicate
    (λ (an-extenor)
      (and (extenor? an-extenor)
           (hash-has-key? (extenor-extenorcl-table an-extenor)
                          this-extenorcl))))
  (define single-field?
    (list-length-one? field-name-spec))
  (define this-extenorcl-getters
    (for/list ([fns field-name-spec]
               [index (in-naturals)])
      (λ (an-extenor)
        (let ([contents
               (hash-ref (extenor-extenorcl-table an-extenor)
                         this-extenorcl
                         (λ () (error 'extenorcl-getter
                                      "extenorcl (~v) not found in extenor (~v)"
                                      name
                                      an-extenor)))])
          (if single-field?
              contents
              (list-ref contents index))))))
  (define this-extenorcl-setters
    (for/list ([fns field-name-spec]
               [index (in-naturals)])
      (λ (an-extenor new-val)
        (when (not (this-extenorcl-predicate an-extenor))
          (error 'extenorcl-setter
                 "extenorcl (~v) not found in extenor (~v)"
                 name
                 an-extenor))
        (if single-field?
            (hash-set (extenor-extenorcl-table an-extenor)
                      this-extenorcl
                      new-val)
            (let ([contents
                   (hash-ref (extenor-extenorcl-table an-extenor)
                             this-extenorcl)])
              (hash-set (extenor-extenorcl-table an-extenor)
                        this-extenorcl
                        (list-set contents index new-val)))))))
  (list
   this-extenorcl
   this-extenorcl-predicate
   this-extenorcl-getters
   this-extenorcl-setters))

(define (make-prop-extenorcl prop prop-val)
  (car (make-extenorcl #:properties (hash prop prop-val))))

(define (list-length-one? l)
  (and (not (null? l))
       (null? (cdr l))))
(define (single-field-extenorcl? ec)
  (list-length-one? (extenorcl-field-spec-list* ec)))

(define opaque-flag (gensym))

(module+ test
  (require rackunit)

  (define-extenorcl point (x y [hidden relevant?]))
  (define-extenorcl proc-return-keys ()
    #:property prop:procedure (λ (self) (get-extenor-keys self)))

  (define my-point
    (add-extenorcl
     (add-extenorcl
      (add-extenorcl empty-extenor
                     point
                     1 2 #t)
      proc-return-keys)
     'z 5))

  (check-equal? (get-extenor-field my-point 'x) 1)
  (check-equal? (point-x my-point) 1)
  (check-equal? (get-extenor-field my-point 'y) 2)
  (check-equal? (point-y my-point) 2)
  (check-equal? (get-extenor-field my-point 'z) 5)
  (check-exn exn? (λ () (get-extenor-field my-point 'relevant?)))
  (check-equal? (get-extenor-field my-point 'not-there 'fallback) 'fallback)
  (check-equal? (get-extenor-field my-point 'not-there (λ () 'fallback-2))
                'fallback-2)
  (check-equal? (point-relevant? my-point) #t)
  (check-equal? (length (my-point)) 3)
  )
