#lang racket

(provide CBuiltin
         CStruct
         CStruct?)

(struct CBuiltin (name)
  #:transparent)
(struct CStruct (name fields)
  #:transparent)
