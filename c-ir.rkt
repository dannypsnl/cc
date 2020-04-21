#lang racket

(require megaparsack)
(require "c-def.rkt")
(require "x64.rkt")

(provide CTop->IR)

(define (expr->IR expr)
  (match expr
    ([CExpr/Int v] (x64/int v))
    ([CExpr/Bool v]
     (match v
       ("true" (x64/int 1))
       ("false" (x64/int 0))))
    ([CExpr/ID _] 'todo-identifier (x64/int 1))
    ([CExpr/Binary _ _ _] 'todo-binary)
    ))

(define (stmt->IR bb boxed-stmt)
  (match (syntax-box-datum boxed-stmt)
    ([CStmt/LocalVarDef _ _ _] 'todo-local-var)
    ([CStmt/Assign _ _] 'todo-assign)
    ([CStmt/Return expr]
     ;;; TODO: use expression size instead of hardcode 32 here
     (emit-to bb (x64/mov 32 (expr->IR expr) (x64/reg "eax")))
     (emit-to bb (x64/ret 64)))
    )
  )

(define (CTop->IR boxed-ctop)
  (match (syntax-box-datum boxed-ctop)
    ([CGlobalVarDef _ _] 'todo-var)
    ([CStructDef _ _] 'todo-struct)
    ([CFuncDef ret-typ name params stmts]
     (let ([bb (x64/block name '())]
           [caller-stack (x64/reg "rbp")])
       ; save caller stack
       (emit-to bb (x64/push 64 caller-stack))
       ; current top-of-stack is bottom of new stack frame
       (emit-to bb (x64/mov 64 (x64/reg "rsp") caller-stack))
       (map
        (lambda (stmt)
          (stmt->IR bb stmt))
        stmts)
       (emit-to bb (x64/pop 64 caller-stack))
       (x64->string bb)))))
