#lang racket

(provide CGlobalVarDef CStructDef CFuncDef)

(struct CGlobalVarDef
  [typ name]
  #:transparent)

(struct CStructDef
  [name fields]
  #:transparent)

(struct CFuncDef
  [ret-typ name params statements]
  #:transparent)
