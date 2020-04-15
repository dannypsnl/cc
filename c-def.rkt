#lang racket

(provide CGlobalVarDef CStructDef)

(struct CGlobalVarDef
  [typ name]
  #:transparent)

(struct CStructDef
  [name fields]
  #:transparent)
