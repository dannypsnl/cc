#lang racket

(require megaparsack)
(require "c-def.rkt"
         "x64.rkt")

(provide CTop->IR new-context)

(struct context
  [; var-name-to-location maps "int x = 1;" such local variable's name(in case is "x") to stack frame location
   ; or global variable, e.g. `int c;` to `_c@GOTPCREL(%rip)`
   var-name-to-location
   parent]
  #:transparent
  #:mutable)
(define (new-context [parent 'no-parent])
  (context (make-hash '()) parent))
(define (context/new-var ctx var location)
  (define var-name (syntax-box-datum var))
  (hash-set! (context-var-name-to-location ctx) var-name location))
(define (context/lookup-var ctx var)
  (define var-name (syntax-box-datum var))
  (hash-ref (context-var-name-to-location ctx) var-name
            (λ ()
              (if (eqv? 'no-parent (context-parent ctx))
                  (raise (format "no variable named: ~a, semantic checker must has a bug" var-name))
                  (context/lookup-var (context-parent ctx) var)))))

(define (expr->IR ctx bb e)
  (define expr (if (syntax-box? e) (syntax-box-datum e) e))
  (match expr
    [(CExpr/Int v) (x64/int 32 v)]
    [(CExpr/Bool v)
     (match v
       ["true" (x64/int 32 1)]
       ["false" (x64/int 32 0)])]
    [(CExpr/ID var-name) (context/lookup-var ctx var-name)]
    [(CExpr/Binary op l-exp r-exp)
     (letrec ([l (expr->IR ctx bb l-exp)]
              [bits (x64/expr->bits l)]
              [r (expr->IR ctx bb r-exp)]
              [eax (x64/reg "eax")])
       (emit-to bb (x64/mov bits l eax))
       (match op
         [#\+
          (emit-to bb (x64/add bits r eax))
          eax]
         [#\-
          (emit-to bb (x64/sub bits r eax))
          eax]
         [#\*
          (emit-to bb (x64/imul bits r eax))
          eax]
         [#\/
          (emit-to bb (x64/cltd))
          (emit-to bb (x64/idiv bits r))
          eax]))]))

(define (stmt->IR ctx bb stack-level boxed-stmt)
  (match (syntax-box-datum boxed-stmt)
    [(CStmt/LocalVarDef _ name expr)
     (let ([location (x64/reg "rbp" (* stack-level -4))]
           [exp (expr->IR ctx bb expr)])
       (context/new-var ctx name location)
       (emit-to bb (x64/mov (x64/expr->bits exp) exp location)))
     (+ stack-level 1)]
    [(CStmt/Assign name expr)
     (let ([location (context/lookup-var ctx name)]
           [exp (expr->IR ctx bb expr)])
       (emit-to bb (x64/mov (x64/expr->bits exp) exp location)))
     stack-level]
    [(CStmt/Return expr)
     (let ([ret-expr (expr->IR ctx bb expr)])
       (emit-to bb (x64/mov (x64/expr->bits ret-expr) ret-expr (x64/reg "eax"))))
     stack-level]
    [(CExpr/Call f arg*)
     ;; the first four argument would be put into registers
     ;; since we only have integer(boolean is integer here too)
     ;; thus the first four arguments would be putted to edi, esi, edx, ecx
     ;; NOTE: floating-number using xmm0, xmm1, xmm2, xmm3 by the way
     ;; TODO: rest arguments would be pushed into stack
     (for ([arg arg*]
           [reg '("edi" "esi" "edx" "ecx")])
       (define exp (expr->IR ctx bb arg))
       (emit-to bb (x64/mov (x64/expr->bits exp) exp (x64/reg reg))))
     (emit-to bb (x64/call 64 (syntax-box-datum (CExpr/ID-v f))))]))

(define (idx->arg/reg index)
  (match index
    [1 (x64/reg "edi")]
    [2 (x64/reg "esi")]
    [3 (x64/reg "edx")]
    [4 (x64/reg "ecx")]
    [5 (x64/reg "r8d")]
    [6 (x64/reg "r9d")]
    [n (x64/reg "rbp" (+ (* 8 (- n 7)) 16))]))

(define (CTop->IR file global-ctx boxed-ctop)
  (match (syntax-box-datum boxed-ctop)
    [(CGlobalVarDef _ name)
     (define n (syntax-box-datum name))
     (context/new-var global-ctx name (x64/global-ref n))
     (add-global-var file (x64/global-var n))]
    [(CStructDef _ _) 'todo-struct]
    [(CFuncDef _ name params stmts)
     (let ([bb (x64/block (syntax-box-datum name) '())]
           [fn-ctx (new-context global-ctx)]
           [caller-stack (x64/reg "rbp")])
       ; save caller stack
       (emit-to bb (x64/push 64 caller-stack))
       ; current top-of-stack is bottom of new stack frame
       (emit-to bb (x64/mov 64 (x64/reg "rsp") caller-stack))
       (define i 1)
       (for ([param params])
         (let ([location (x64/reg "rbp" (* i -4))])
           ;; TODO: from gcc output can see that parameter after 7 is not allocated on stack frame but %eax, %r10d, %r11d
           (emit-to bb (x64/mov 32
                                (idx->arg/reg i)
                                location))
           (set! i (+ 1 i))
           (match param
             [(list _ name)
              (context/new-var fn-ctx
                               name
                               location)])))
       (for ([stmt stmts])
         (set! i (stmt->IR fn-ctx bb i stmt)))
       (emit-to bb (x64/pop 64 caller-stack))
       (emit-to bb (x64/ret 64))
       (add-func file bb))]))
