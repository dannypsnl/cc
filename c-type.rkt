#lang racket

(provide CBuiltin
         CStruct
         CStruct?)

(struct CBuiltin ()
  #:transparent)
(struct CStruct ([fields])
  #:transparent)
