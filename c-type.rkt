#lang racket

(provide (struct-out CBuiltin)
         (struct-out CStruct)
         (struct-out CFunction)
         type-definition->string)

(struct CBuiltin (name)
  #:transparent)
(struct CStruct (name fields)
  #:transparent)
(struct CFunction (ret param*)
  #:transparent)

(define (type-definition->string typ-def)
  (match typ-def
    ([CBuiltin name] name)
    ([CStruct name _] (format "struct ~a" name))))
