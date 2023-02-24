#lang info
(define deps '("base"
               "rackunit-lib"
               ))
(define build-deps '("sandbox-lib"
                     "scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/extenor.scrbl" () (library))))
(define license '(Apache-2.0 OR MIT))

