#lang racket

(provide CBuiltin
         CStruct
         CStruct?
         type-definition->string)

(struct CBuiltin (name)
  #:transparent)
(struct CStruct (name fields)
  #:transparent)

(define (type-definition->string typ-def)
  (match typ-def
    ([CBuiltin name] name)
    ([CStruct name _] (format "struct ~a" name))))