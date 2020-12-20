#lang racket/base

(provide (rename-out [basic-prop:dict-extenorcl prop:dict-extenorcl]))

(require
 "../main.rkt"
 racket/match
 racket/dict
 )

(define (extenor-basic-dict-count extenor)
  (length (extenor-keys extenor)))

(define (extenor-basic-dict-iterate-first extenor)
  (define iter (extenor-keys extenor))
  (if (null? iter) #f iter))

(define (extenor-basic-dict-iterate-next extenor prev-iter)
  (define next-iter (cdr prev-iter))
  (if (null? next-iter)
      #f
      next-iter))

(define (extenor-basic-dict-iterate-key extenor iter)
  (car iter))

(define (extenor-basic-dict-iterate-value extenor iter)
  (extenor-ref (car iter)))

(define bad-result-flag (gensym))

(define (extenor-basic-dict-remove extenor key)
  (error 'extenor-dict-remove "TODO - implement dict-remove for extenors"))

(define basic-prop:dict-extenorcl
  (make-prop-extenorcl
   prop:dict
   (vector-immutable
    extenor-ref
    #f ;dict-set!
    extenor-set
    #f ;dict-remove!
    extenor-basic-dict-remove
    extenor-basic-dict-count
    extenor-basic-dict-iterate-first
    extenor-basic-dict-iterate-next
    extenor-basic-dict-iterate-key
    extenor-basic-dict-iterate-value)))


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

  (define my-point-dict
    (extenor-extend my-point basic-prop:dict-extenorcl))

  (check-equal? (dict-ref my-point-dict 'x) 1)
  (check-equal? (dict-ref my-point-dict 'z) 5)
  (check-equal? (dict-ref my-point-dict 'something-not-there 'fallback) 'fallback)
  (define mpd-1 (dict-set my-point-dict 'foobar 'awesome-sauce))
  (check-equal? (dict-ref mpd-1 'foobar) 'awesome-sauce)
  ;; TODO - test iterator interface, maybe with a for loop and in-dict

  )
