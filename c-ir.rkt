#lang racket

(require megaparsack)
(require "c-def.rkt")
(require "x64.rkt")

(provide CTop->IR)

(struct local-context
  [; var-name-to-location maps "int x = 1;" such local variable's name(in case is "x") to stack frame location
   var-name-to-location]
  #:transparent
  #:mutable)
(define (new-context)
  (local-context (make-hash '())))
(define (context/new-var ctx var-name location)
  (hash-set! (local-context-var-name-to-location ctx) var-name location))
(define (context/lookup-var ctx var-name)
  (hash-ref (local-context-var-name-to-location ctx) var-name))

(define (expr->IR ctx expr)
  (match expr
    ([CExpr/Int v] (x64/int 32 v))
    ([CExpr/Bool v]
     (match v
       ("true" (x64/int 32 1))
       ("false" (x64/int 32 0))))
    ([CExpr/ID var-name] (context/lookup-var ctx var-name))
    ([CExpr/Binary _ _ _] 'todo-binary)))

(define (stmt->IR ctx bb stack-level boxed-stmt)
  (match (syntax-box-datum boxed-stmt)
    ([CStmt/LocalVarDef _ name expr]
     (let ([location (x64/reg "rbp" (* stack-level -4))]
           [exp (expr->IR ctx expr)])
       (context/new-var ctx name location)
       (emit-to bb (x64/mov (x64/expr->bits exp) exp location)))
     (+ stack-level 1))
    ([CStmt/Assign name expr]
     (let ([location (context/lookup-var ctx name)]
           [exp (expr->IR ctx expr)])
       (emit-to bb (x64/mov (x64/expr->bits exp) exp location)))
     stack-level)
    ([CStmt/Return expr]
     (let ([ret-expr (expr->IR ctx expr)])
       (emit-to bb (x64/mov (x64/expr->bits ret-expr) ret-expr (x64/reg "eax"))))
     stack-level)))

; TODO: work with more arguments
(define (idx->arg/reg index)
  (match index
    (1 (x64/reg "edi"))
    (2 (x64/reg "esi"))
    (3 (x64/reg "edx"))
    (4 (x64/reg "ecx"))
    (5 (x64/reg "r8d"))
    (6 (x64/reg "r9d"))))

(define (CTop->IR boxed-ctop)
  (match (syntax-box-datum boxed-ctop)
    ([CGlobalVarDef _ _] 'todo-var)
    ([CStructDef _ _] 'todo-struct)
    ([CFuncDef ret-typ name params stmts]
     (let ([bb (x64/block name '())]
           [fn-ctx (new-context)]
           [caller-stack (x64/reg "rbp")])
       ; save caller stack
       (emit-to bb (x64/push 64 caller-stack))
       ; current top-of-stack is bottom of new stack frame
       (emit-to bb (x64/mov 64 (x64/reg "rsp") caller-stack))
       (define i 1)
       (map (λ (param)
              (let ([location (x64/reg "rbp" (* i -4))])
                (emit-to bb (x64/mov 32
                                     (idx->arg/reg i)
                                     location))
                (set! i (+ 1 i))
                (context/new-var fn-ctx
                                 ; param name
                                 (car (reverse param))
                                 location)))
            params)
       (map (λ (stmt)
              (set! i (stmt->IR fn-ctx bb i stmt)))
            stmts)
       (emit-to bb (x64/pop 64 caller-stack))
       (emit-to bb (x64/ret 64))
       (x64->string bb)))))
