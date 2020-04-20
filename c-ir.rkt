#lang racket

(require megaparsack)
(require "c-def.rkt")
(require "x64.rkt")

(provide CTop->IR)

(define (stmt->IR boxed-stmt)
  (match (syntax-box-datum boxed-stmt)
    ([CStmt/LocalVarDef _ _ _] 'todo-local-var)
    ([CStmt/Assign _ _] 'todo-assign)
    ([CStmt/Return expr]
     ;;; TODO: mov expr to rbp
     (x64/retq))
    )
  )

(define (CTop->IR boxed-ctop)
  (match (syntax-box-datum boxed-ctop)
    ([CGlobalVarDef _ _] 'todo-var)
    ([CStructDef _ _] 'todo-struct)
    ([CFuncDef ret-typ name params stmts]
     (let ([bb (x64/block name '())])
       (map
        (lambda (stmt)
          (emit-to bb (stmt->IR stmt)))
        stmts)
       (x64->string bb)))))