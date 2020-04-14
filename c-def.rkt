#lang racket

(provide CGlobalVarDef)

(struct CGlobalVarDef
  [typ name]
  #:transparent)
