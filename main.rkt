#lang racket/base

(require racket/contract)
(define (isym? x) (and (symbol? x) (symbol-interned? x)))

(provide
 (contract-out
  [extenor? (-> any/c any/c)]
  [empty-extenor extenor?]
  [extenor-extend (->* (extenor? extenorcl?*)
                       ()
                       #:rest (listof any/c)
                       extenor?)]
  [extenor-ref (->* (extenor? isym?) (any/c) any/c)]
  [extenor-set (-> extenor? isym? any/c any/c)]
  [extenor-keys (-> extenor? (listof isym?))]
  [extenor-has-key? (-> extenor? isym? any/c)]
  [extenor-struct-type-properties (-> extenor? (listof struct-type-property?))]
  [extenor-remove-extenorcl (-> extenor? extenorcl?* extenor?)]
  [extenor-remove-extenorcl-with-key (-> extenor? isym? extenor?)]
  [extenor-remove-extenorcl-with-struct-type-property
   (-> extenor? struct-type-property? extenor?)]
  [extenor-simple-merge (->* (extenor? extenor?)
                             (#:equality (or/c #t #f (-> any/c any/c any/c)))
                             extenor?)]
  ; TODO - extenor-merge

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
  [extenorcl-struct-type-properties (-> extenorcl?* (listof struct-type-property?))]
  [rename extenorcl-names* extenorcl-keys (-> extenorcl?* (listof symbol?))]
  [make-prop-extenorcl (-> struct-type-property? any/c extenorcl?)]
  )
 define-extenorcl
 )


(require
 racket/list
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

;; I may want to change what kind of dictionary I'm using.
;; I don't want extra cost for generics, so I'm just going to use my own names.
(define t-ref hash-ref)
(define t-set hash-set)
(define t-empty (hasheq))
(define t-keys hash-keys)
(define t-has? hash-has-key?)
(define t-remove hash-remove)

;; An extenor has:
;; * The struct-type constructor for its type (for quickly setting fields without
;;   defining a new struct-type with all the properties).
;; * A set of extenorcls contained, as a table from extenorcl to #t.
;; * A table of visible names (where each name is an interned symbol).
;; * A table of hidden names (where each name is an uninterned symbol).
;;   The values of both name tables are a pair of (cons extenorcl value),
;;   so the extenorcrl that hosts the name is easy to access.
;; * An alist of struct-type-properties (with their values as RHS).
;;
;; Extenors can be extended with more extenorcls. If an extenorcl adds
;; a new property, the extended extenor must use a new subtype of the
;; base extenor struct that includes the property. If the extenorcl does
;; not add a new property, then the current struct constructor can be
;; re-used.
(struct extenor
  (struct-constructor extenorcl-table visible-name-table hidden-name-table stp-alist))
(define empty-extenor (extenor extenor t-empty t-empty t-empty '()))

;; extenorcls have:
;; * name - a name for the extenorcl (may be #f or interned symbol)
;; * names - a list of names where visibles are interned symbols and hiddens are uninterned symbols.
;; * visibles - like names list but with only visibles.
;; * property-alist - mapping struct-type-properties to their values.
;; * a guard, which is either #f or a function that accepts one argument for
;;   each field and returns a list of the same number of fields.  It can
;;   either raise an exception to block construction with bad data or it
;;   can transform the data for construction.
(struct extenorcl (name all-names visibles property-alist guard))

(define (extenorcl?* ec)
  (or (isym? ec) (extenorcl? ec)))
(define (extenorcl-name* ec)
  (if (symbol? ec)
      ec
      (extenorcl-name ec)))
(define (extenorcl-names* ec)
  (if (symbol? ec)
      (list ec)
      (extenorcl-visibles ec)))
(define (extenorcl-all-names* ec)
  (if (symbol? ec)
      (list ec)
      (extenorcl-all-names ec)))
(define (extenorcl-property-alist* ec)
  (if (symbol? ec)
      '()
      (extenorcl-property-alist ec)))
(define (extenorcl-guard* ec)
  (if (symbol? ec)
      #f
      (extenorcl-guard ec)))


(define (build-extenor-constructor prop-alist)
  (define-values (type constructor predicate accessor mutator)
    (make-struct-type (gensym) struct:extenor 0 0 0 prop-alist))
  constructor)

(define (extenor-extend an-extenor an-extenorcl . field-vals)
  ;; Check whether the extenorcl is already there.
  (when (t-has? (extenor-extenorcl-table an-extenor) an-extenorcl)
    (error 'extenor-extend "the extenor already contains the extenorcl: ~v" an-extenorcl))
  ;; Check whether any visible fields of the extenorcl conflict with existing
  ;; visible fields in the extenor.
  (define new-visibles (extenorcl-names* an-extenorcl))
  (define old-visibles-table (extenor-visible-name-table an-extenor))
  (define field-conflict
    (for/or ([vis new-visibles])
      (and (t-has? old-visibles-table vis) vis)))
  (when field-conflict
    (error 'extenor-extend "extenor already contains visible field: ~v"
           field-conflict))
  ;; Check whether any properties of the extenorcl conflict with existing properties
  ;; on the extenor.
  (define new-props-alist (extenorcl-property-alist* an-extenorcl))
  (define old-props-alist (extenor-stp-alist an-extenor))
  (define prop-conflict
    (for/or ([prop (map car new-props-alist)])
      (and (assq prop old-props-alist)
           prop)))
  (when prop-conflict
    (error 'extenor-extend "extenor already contains struct-type-property: ~v"
           prop-conflict))
  ;; If there are new properties, we need to make a new struct-type
  ;; that is a subtype of extenor that has all struct-type-properties
  ;; of all extenorcls.
  (define joined-props-alist (append new-props-alist old-props-alist))
  (define new-constructor
    (if (null? new-props-alist)
        (extenor-struct-constructor an-extenor)
        (build-extenor-constructor joined-props-alist)))

  ;; We need to add any new fields, applying guards as necessary.
  (define field-vals-l (length field-vals))
  (define field-specs-l (length (extenorcl-all-names* an-extenorcl)))
  (when (not (eq? field-vals-l
                  field-specs-l))
    (error 'extenor-extend "Improper number of field values.  Expected ~a, received ~a"
           field-specs-l
           field-vals-l))
  (define guarded-field-vals
    (if (extenorcl-guard* an-extenorcl)
        (apply (extenorcl-guard* an-extenorcl) field-vals)
        field-vals))
  (when (and (not (eq? field-vals guarded-field-vals))
             (not (eq? field-vals-l (length guarded-field-vals))))
    (error 'extenor-extend
           "Guard procedure returned the wrong number of fields for extenorcl: ~v"
           (extenorcl-name* an-extenorcl)))

  (define-values (new-visible-table new-hidden-table)
    (for/fold ([vistab (extenor-visible-name-table an-extenor)]
               [hidtab (extenor-hidden-name-table an-extenor)])
              ([n (extenorcl-all-names* an-extenorcl)]
               [v guarded-field-vals])
      (values
       (if (symbol-interned? n)
           (values (t-set vistab n (cons an-extenorcl v)) hidtab)
           (values vistab (t-set hidtab n (cons an-extenorcl v)))))))
  (new-constructor new-constructor
                   (t-set (extenor-extenorcl-table an-extenor) an-extenorcl #t)
                   new-visible-table
                   new-hidden-table
                   joined-props-alist))

(define (extenor-keys an-extenor)
  (t-keys (extenor-visible-name-table an-extenor)))
(define (extenor-has-key? an-extenor name)
  (t-has? (extenor-visible-name-table an-extenor) name))

(define (extenor-remove-extenorcl an-extenor an-extenorcl)
  (define extenor-constructor (extenor-struct-constructor an-extenor))
  (define new-cl-table (t-remove (extenor-extenorcl-table an-extenor) an-extenorcl))
  (define-values (new-visible-table new-hidden-table)
    (for/fold ([vistab (extenor-visible-name-table an-extenor)]
               [hidtab (extenor-hidden-name-table an-extenor)])
              ([n (extenorcl-all-names* an-extenorcl)])
      (if (symbol-interned? n)
          (values (t-remove vistab n) hidtab)
          (values vistab (t-remove hidtab n)))))
  (define new-props-alist
    (if (null? (extenorcl-property-alist* an-extenorcl))
        (extenor-stp-alist an-extenor)
        (for/fold ([props '()])
                  ([extenorcl (t-keys new-cl-table)])
          (if (symbol? extenorcl)
              props
              (append (extenorcl-property-alist* extenorcl)
                      props)))))
  (define new-constructor
    (if (null? (extenorcl-property-alist* an-extenorcl))
        extenor-constructor
        (build-extenor-constructor new-props-alist)))

  (new-constructor new-constructor
                   new-cl-table
                   new-visible-table
                   new-hidden-table
                   new-props-alist))

(define (extenor-remove-extenorcl-with-key an-extenor name)
  (define ref (t-ref (extenor-visible-name-table an-extenor) name #f))
  (when (not ref)
    (error 'extenor-remove-extenorcl-with-name
           "Key not found in extenor: ~v"
           name))
  (extenor-remove-extenorcl an-extenor (car ref)))

(define (extenor-remove-extenorcl-with-struct-type-property an-extenor stp)
  (define the-extenorcl
    (for/or ([cl (t-keys (extenor-extenorcl-table an-extenor))])
      (and (assq stp (extenorcl-property-alist* cl))
           cl)))
  (when (not the-extenorcl)
    (error 'extenor-remove-extenorcl-with-structure-type-property
           "structure-type-property not found in extenor: ~v"
           stp))
  (extenor-remove-extenorcl an-extenor the-extenorcl))


(define (extenor-ref an-extenor field-symbol
                     [fallback (λ () (error 'extenor-ref
                                            "No value found for key: ~v"
                                            field-symbol))])
  ;; name may be visible (interned) or hidden for internal use, but I'll export
  ;; this function with a contract that only accepts visible names.
  (define bare-result
    (if (symbol-interned? field-symbol)
        (t-ref (extenor-visible-name-table an-extenor) field-symbol #f)
        (t-ref (extenor-hidden-name-table an-extenor) field-symbol #f)))
  (if (not bare-result)
      (if (procedure? fallback)
          (fallback)
          fallback)
      (cdr bare-result)))

(define (extenor-set an-extenor name new-value)
  ;; name may be visible (interned) or hidden for internal use, but I'll export
  ;; this function with a contract that only accepts visible names.
  (define ctor (extenor-struct-constructor an-extenor))
  (define vis (extenor-visible-name-table an-extenor))
  (define hid (extenor-hidden-name-table an-extenor))
  (define name-is-vis? (symbol-interned? name))
  (define bare-ref
    (if name-is-vis?
        (t-ref vis name #f)
        (t-ref hid name #f)))
  (cond
    [(not bare-ref)
     ;; If the key is not there, we add a fresh symbol extenorc.
;(struct-constructor extenorcl-table visible-name-table hidden-name-table stp-alist)
     (ctor ctor
           (t-set (extenor-extenorcl-table an-extenor) name #t)
           (t-set vis name (cons name new-value))
           (extenor-hidden-name-table an-extenor)
           (extenor-stp-alist an-extenor))]
    [(symbol? (car bare-ref))
     ;; If it's a degenerate symbol extenorcl, we don't need to worry about guards.
     (ctor ctor
           (extenor-extenorcl-table an-extenor)
           (t-set vis name (cons name new-value))
           (extenor-hidden-name-table an-extenor)
           (extenor-stp-alist an-extenor))]
    [else
     (define the-extenorcl (car bare-ref))
     (define guard (extenorcl-guard the-extenorcl))
     (if guard
         (let* ([all-names (extenorcl-all-names* the-extenorcl)]
                [new-val-list (for/list ([n all-names])
                                (if (eq? n name)
                                    new-value
                                    (if (symbol-interned? n)
                                        (cdr (t-ref vis n))
                                        (cdr (t-ref hid n)))))]
                [guarded-val-list (apply guard new-val-list)])
           (define-values (new-vis new-hid)
             (for/fold ([vis vis]
                        [hid hid])
                       ([n all-names]
                        [v guarded-val-list])
               (if (symbol-interned? n)
                   (values (t-set vis n (cons the-extenorcl v)) hid)
                   (values vis (t-set hid n (cons the-extenorcl v))))))
           (ctor ctor
                 (extenor-extenorcl-table an-extenor)
                 new-vis
                 new-hid
                 (extenor-stp-alist an-extenor)))
         (ctor ctor
               (extenor-extenorcl-table an-extenor)
               (if name-is-vis?
                   (t-set vis name (cons the-extenorcl new-value))
                   vis)
               (if name-is-vis?
                   hid
                   (t-set hid name (cons the-extenorcl new-value)))
               (extenor-stp-alist an-extenor)))]))

(define (extenor-struct-type-properties an-extenor)
  (map car (extenor-stp-alist an-extenor)))
(define (extenorcl-struct-type-properties an-extenorcl)
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
  (define all-names (map (λ (ns) (if (eq? 'hidden (car ns))
                                     (string->uninterned-symbol
                                      (symbol->string (cdr ns)))
                                     (cdr ns)))
                         field-name-spec))
  (define visible-names (filter symbol-interned? all-names))
  (define this-extenorcl
    (extenorcl name
               all-names
               visible-names
               (for/list ([(key val) (in-hash properties)])
                 #;(when (not (struct-type-property? key))
                     (error 'make-extenorcl "Not a struct-type-property: ~v" key))
                 (cons key val))
               guard))
  (define this-extenorcl-predicate
    (λ (an-extenor)
      (and (extenor? an-extenor)
           (t-has? (extenor-extenorcl-table an-extenor)
                   this-extenorcl))))
  (define this-extenorcl-getters
    (for/list ([n all-names])
      (λ (an-extenor)
        (if (this-extenorcl-predicate an-extenor)
            (extenor-ref an-extenor n)
            (error 'extenorcl-getter
                   "extenor (~v) is not an instance of extenorcl (~v)"
                   an-extenor
                   (or name this-extenorcl))))))
  (define this-extenorcl-setters
    (for/list ([n all-names])
      (λ (an-extenor new-val)
        (if (this-extenorcl-predicate an-extenor)
            (extenor-set an-extenor n new-val)
            (error 'extenorcl-setter
                   "extenor (~v) is not an instance of extenorcl (~v)"
                   an-extenor
                   (or name this-extenorcl))))))
  (list
   this-extenorcl
   this-extenorcl-predicate
   this-extenorcl-getters
   this-extenorcl-setters))

(define (make-prop-extenorcl prop prop-val)
  (car (make-extenorcl #:name (format "~a_prop-extenorcl" (object-name prop))
                       #:properties (hash prop prop-val))))

(define opaque-flag (gensym))



(define (extenor-simple-merge l r #:equality [equality equal?])
  ;; If l and r have different extenorcls that claim the same field, raise an exception.
  ;; If both extenors have the same field, and equality returns false, raise an exception.
  ;; If both extenors have the same field, and equality returns true, use the value in r.
  (define l-vis-keys (extenor-keys l))
  (define l-vis-tab (extenor-visible-name-table l))
  (define r-vis-keys (extenor-keys r))
  (define r-vis-tab (extenor-visible-name-table r))
  (define new-visible-table
    (for/fold ([new-vis r-vis-tab])
              ([lk l-vis-keys])
      (define l-ref (t-ref l-vis-tab lk #f))
      (define r-ref (t-ref r-vis-tab lk #f))
      (cond [(not r-ref) (t-set new-vis lk l-ref)]
            [(not (eq? (car l-ref) (car r-ref)))
             (error 'extenor-simple-merge
                    "extenors have differernt extenorcls providing field ~a"
                    lk)]
            [(or (not equality)
                 (and (procedure? equality)
                      (not (equality (cdr l-ref) (cdr r-ref)))))
             (error 'extenor-simple-merge
                    "extenors have conflicting values for field ~a: ~v and ~v"
                    lk
                    (cdr l-ref) (cdr r-ref))]
            [else new-vis])))

  (define l-hid-tab (extenor-hidden-name-table l))
  (define r-hid-tab (extenor-hidden-name-table r))
  (define new-hidden-table
    (for/fold ([new-hid r-hid-tab])
              ([lk (t-keys l-hid-tab)])
      (define l-ref (t-ref l-hid-tab lk #f))
      (define r-ref (t-ref r-hid-tab lk #f))
      (cond [(not r-ref) (t-set new-hid lk l-ref)]
            ;; Hidden field names can't conflict, so we have one less case than with visible fields..
            [(or (not equality)
                 (and (procedure? equality)
                      (not (equality (cdr l-ref) (cdr r-ref)))))
             (error 'extenor-simple-merge
                    "extenors have conflicting values for hidden field ~a: ~v and ~v"
                    lk
                    (cdr l-ref) (cdr r-ref))]
            [else new-hid])))

  (define l-props (extenor-stp-alist l))
  (define r-props (extenor-stp-alist r))
  (define new-props-alist
    (for/fold ([new-props r-props])
              ([l-prop-pair l-props])
      (define r-prop-pair (assq (car l-prop-pair) r-props))
      (cond [(not r-prop-pair) (cons l-prop-pair new-props)]
            [(or (not equality)
                 (and (procedure? equality)
                      (not (equality (cdr l-prop-pair) (cdr r-prop-pair)))))
             (error 'extenor-simple-merge
                    "extenors have conflicting values for struct-type-property ~v: ~v and ~v"
                    (car l-prop-pair)
                    (cdr l-prop-pair) (cdr r-prop-pair))]
            ;; TODO - fail if properties come from different extenorcls.
            ;;        Probably I should change the storage of properties to be like
            ;;        the other fields, a table from prop to (cons extenorcl prop-val).
            ;;        This makes things more uniform, and just make constructor
            ;;        construction slightly more complicated.
            [else new-props])))

  (define ctor (if (eq? new-props-alist r-props)
                   (extenor-struct-constructor r)
                   (build-extenor-constructor new-props-alist)))

  (ctor ctor
        (for/fold ([extenorcl-table (extenor-extenorcl-table r)])
                  ([cl (t-keys (extenor-extenorcl-table l))])
          (t-set extenorcl-table cl #t))
        new-visible-table
        new-hidden-table
        new-props-alist))



(module+ test
  (require rackunit)

  (define-extenorcl point (x y [hidden relevant?]))
  (define-extenorcl proc-return-keys ()
    #:property prop:procedure (λ (self) (extenor-keys self)))

  (define my-point
    (extenor-extend
     (extenor-extend
      (extenor-extend empty-extenor
                      point
                      1 2 #t)
      proc-return-keys)
     'z 5))

  (check-equal? (extenor-ref my-point 'x) 1)
  (check-equal? (point-x my-point) 1)
  (check-equal? (extenor-ref my-point 'y) 2)
  (check-equal? (point-y my-point) 2)
  (check-equal? (extenor-ref my-point 'z) 5)
  (check-exn exn? (λ () (extenor-ref my-point 'relevant?)))
  (check-equal? (extenor-ref my-point 'not-there 'fallback) 'fallback)
  (check-equal? (extenor-ref my-point 'not-there (λ () 'fallback-2))
                'fallback-2)
  (check-equal? (point-relevant? my-point) #t)
  (check-equal? (length (my-point)) 3)
  (check-equal? (extenor-ref (extenor-remove-extenorcl my-point 'z) 'z 'fallback)
                'fallback)
  (check-equal? (extenor-ref (extenor-remove-extenorcl my-point point) 'x 'fallback)
                'fallback)
  (check-equal? (extenor-ref (extenor-remove-extenorcl-with-key my-point 'x)
                             'y 'fallback)
                'fallback)
  (check-exn exn? (λ () ((extenor-remove-extenorcl-with-struct-type-property
                          my-point prop:procedure))))
  (check-equal? (extenor-struct-type-properties my-point)
                (list prop:procedure))

  (define my-point-2 (extenor-extend empty-extenor point 3 4 #f))

  ;; Same field value, but field comes from different extenorcls
  (check-exn exn? (λ () (extenor-simple-merge my-point-2
                                              (extenor-extend empty-extenor 'x 3))))
  ;; No conflicting extenorcls, but unequal fields.
  (check-exn exn? (λ () (extenor-simple-merge my-point my-point-2)))
  ;; Successful merge on simple symbol extenorcl
  (check-equal? (extenor-ref (extenor-simple-merge
                              my-point
                              (extenor-extend empty-extenor 'z 5))
                             'z)
                5)

  ;; successful merge
  (define my-point-merge-1 (extenor-simple-merge my-point my-point-2
                                                 #:equality (λ (l r) #t)))
  (define my-point-merge-2 (extenor-simple-merge my-point my-point-2
                                                 #:equality #t))
  (check-equal? (extenor-ref my-point-merge-1 'x)
                3)
  (check-equal? (extenor-ref my-point-merge-2 'x)
                3)
  (check-equal? (point-relevant? my-point-merge-1)
                #f)
  (check-equal? (point-relevant? my-point-merge-2)
                #f)
  (check-equal? (procedure? my-point-merge-1) #t)
  (check-equal? (procedure? my-point-merge-2) #t)
  )
