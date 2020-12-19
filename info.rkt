#lang info
(define collection "cc")
(define deps '("base"
               "reporter"
               "megaparsack-lib"
               "functional-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/cc.scrbl" ())))
(define pkg-desc "A compiler of C subset")
(define version "0.0")
(define pkg-authors '(dannypsnl))
