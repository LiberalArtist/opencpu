#lang info
(define collection "opencpu")
(define deps '("base"
               "rackunit-lib"
               "adjutor"))
(define build-deps '("scribble-lib" "racket-doc" "net-doc"))
(define scribblings '(("scribblings/opencpu.scrbl" ())))
(define pkg-desc "A Racket interface to R functions using the OpenCPU API")
(define version "0.0")
(define pkg-authors '(philip))
