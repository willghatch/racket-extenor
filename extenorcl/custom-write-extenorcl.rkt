#lang racket/base
(provide prop:custom-write-extenorcl)
(require "../main.rkt")
(define prop:custom-write-extenorcl
  (make-prop-extenorcl
   prop:custom-write
   (λ (self port mode)
     ;; mode is #t for write, #f for display, 0 or 1 as the quoting depth for print
     (define keys (extenor-names self))
     (fprintf port "#extenor<~a>"
              (string-join (map (λ (k) (format "~a:~v" k (extenor-ref self k)))
                                keys)
                           ", ")))))
