#lang racket/base

(provide
 prop:custom-write-extenorc
 basic-prop:dict-extenorc
 )

(require
 "extenor.rkt"
 racket/string
 racket/match
 (submod "extenor.rkt" for-private)
 racket/dict
 )

(define prop:custom-write-extenorc
  (make-prop-extenorc
   prop:custom-write
   (λ (self port mode)
     ;; mode is #t for write, #f for display, 0 or 1 as the quoting depth for print
     (define keys (get-extenor-keys self))
     (fprintf port "#extenor<~a>"
              (string-join (map (λ (k) (format "~a:~v" k (get-extenor-field self k)))
                                keys)
                           ", ")))))

#;(define hasheq-extenorc
  (make-prop-extenorc
   prop:dict
   (vector-immutable
    dict-ref
    #f ;dict-set!
    dict-set
    #f ;dict-remove!
    dict-remove
    dict-count
    ;; dict-iterate-first returns some kind of index to the first thing in the hash
    dict-iterate-first
    ;; dict-iterate-next takes a previous index (eg from dict-iterate-next or dict-iterate-first) and returns the next one, hopefully quickly, until there are no more, at which point it returns #f.
    dict-iterate-next
    ;; dict-iterate-key returns the key at a given iteration point
    dict-iterate-key
    ;; dict-iterate-value returns the value at a given iteration point
    dict-iterate-value)))

(define (extenor-basic-dict-count extenor)
  (for/fold ([result 0])
            ([extenorc (in-hash-keys (extenor-extenorc-table extenor))])
    (+ result
       (length (filter (λ (x) (eq? (car x 'visible)))
                       (extenorc-field-spec-list* extenorc))))))

(define (extenor-basic-dict-iterate/generic extenor previous-iter)
  (define-values (previous-index previous-jndex)
    (match previous-iter
      [(cons (? exact-integer?) (? exact-integer?))
       (values (car previous-iter) (cdr previous-iter))]
      [#f (values #f #f)]))
  (for/fold ([result #f])
            ([extenorc (in-hash-keys (extenor-extenorc-table extenor))]
             [index (in-naturals)]
             #:unless (and previous-index (<= index previous-index)))
    #:break result
    (and (not (symbol? extenorc))
         (for/fold ([result result])
                   ([field-spec (extenorc-field-spec-list* extenorc)]
                    [jndex (in-naturals)]
                    #:unless (and previous-jndex (<= jndex previous-jndex)))
           #:break result
           (and (eq? (car field-spec) 'visible)
                (cons index jndex))))))

;; These dict functions all have bad complexity, but I want to get something working before worrying about performance.
(define (extenor-basic-dict-iterate-first extenor)
  (extenor-basic-dict-iterate/generic extenor #f))

(define (extenor-basic-dict-iterate-next extenor prev-pos)
  (extenor-basic-dict-iterate/generic extenor prev-pos))

(define bad-result-flag (gensym))

(define (extenor-basic-dict-iterate-key extenor iteration-key)
  (define tentative
    (match iteration-key
      [(cons (and a (? exact-integer?)) (and b (? exact-integer?)))
       (for/fold ([result opaque-flag])
                 ([extenorc (in-hash-keys (extenor-extenorc-table extenor))]
                  [index (in-naturals)]
                  #:when (equal? index a))
         #:break (not (eq? result opaque-flag))
         (for/fold ([result result])
                   ([field-spec (extenorc-field-spec-list* extenorc)]
                    [jndex (in-naturals)]
                    #:when (equal? jndex b))
           #:break (not (eq? result opaque-flag))
           (if (eq? (car field-spec) 'visible)
               (cdr field-spec)
               bad-result-flag)))]))
  (if (memq tentative (list bad-result-flag opaque-flag))
      (error 'extenor-dict-iterate-key "bad iteration key")
      tentative))

(define (extenor-basic-dict-iterate-value extenor iteration-key)
  (define tentative
    (match iteration-key
      [(cons (and a (? exact-integer?)) (and b (? exact-integer?)))
       (for/fold ([result opaque-flag])
                 ([extenorc (in-hash-keys (extenor-extenorc-table extenor))]
                  [index (in-naturals)]
                  #:when (equal? index a))
         #:break (not (eq? result opaque-flag))
         (define contents (hash-ref (extenor-extenorc-table extenor) extenorc))
         (if (and (single-field-extenorc? extenorc) (eq? index 0))
             (and (eq? (car (extenorc-field-spec-list* extenorc))
                       'visible)
                  contents)
             (for/fold ([result result])
                       ([field-val contents]
                        [field-spec (extenorc-field-spec-list* extenorc)]
                        [jndex (in-naturals)]
                        #:when (equal? jndex b))
               #:break (not (eq? result opaque-flag))
               (if (eq? (car field-spec) 'visible)
                   field-val
                   bad-result-flag))))]))
  (if (memq tentative (list bad-result-flag opaque-flag))
      (error 'extenor-dict-iterate-value "bad iteration key")
      tentative))

(define (extenor-basic-dict-remove extenor key)
  (error 'extenor-dict-remove "TODO - implement dict-remove for extenors"))

(define basic-prop:dict-extenorc
  (make-prop-extenorc
   prop:dict
   (vector-immutable
    get-extenor-field
    #f ;dict-set!
    set-extenor-field
    #f ;dict-remove!
    extenor-basic-dict-remove
    extenor-basic-dict-count
    extenor-basic-dict-iterate-first
    extenor-basic-dict-iterate-next
    extenor-basic-dict-iterate-key
    extenor-basic-dict-iterate-value)))


(module+ test
  (require rackunit)

  (define-extenorc point (x y [hidden relevant?]))
  (define-extenorc proc-return-keys ()
    #:property prop:procedure (λ (self) (get-extenor-keys self)))

  (define my-point
    (add-extenorc
     (add-extenorc
      (add-extenorc empty-extenor
                    point
                    1 2 #t)
      proc-return-keys)
     'z 5))

  (define my-point-dict
    (add-extenorc my-point basic-prop:dict-extenorc))

  (check-equal? (dict-ref my-point-dict 'x) 1)
  (check-equal? (dict-ref my-point-dict 'z) 5)
  (check-equal? (dict-ref my-point-dict 'something-not-there 'fallback) 'fallback)
  (define mpd-1 (dict-set my-point-dict 'foobar 'awesome-sauce))
  (check-equal? (dict-ref mpd-1 'foobar) 'awesome-sauce)
  ;; TODO - test iterator interface, maybe with a for loop and in-dict

  )
