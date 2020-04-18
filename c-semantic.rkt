#lang racket

(require "c-def.rkt")
(require "c-context.rkt")

(provide checker/new
         checker/check-ctop
         checker/report)

(struct checker
  [ctx
   rules]
  #:mutable)

(define (checker/new ctx)
  (checker ctx '()))

(define (checker/add-rule checker rule)
  (set-checker-rules! checker (append (checker-rules checker) (list rule))))
(define (checker/report checker)
  (map
   (lambda (rule)
     (context/execute-rule
      (checker-ctx checker)
      rule))
   (checker-rules checker)))

(define (checker/check-ctop checker prog)
  (match prog
    ([CGlobalVarDef typ name]
     (checker/add-rule checker (rule/bind name typ)))
    ([CStructDef _ _] 'ignore)
    ([CFuncDef ret-typ name params stmts]
     (map
      (lambda (param)
        (checker/add-rule checker (rule/bind (car (reverse param)) (car param))))
      params)
     (checker/add-rule checker (rule/push-env))
     (checker/check-body checker ret-typ stmts)
     (checker/add-rule checker (rule/pop-env)))))

(define (checker/check-body checker ret-typ statements)
  (map
   (lambda (stmt)
     (checker/check-stmt checker ret-typ stmt))
   statements))

(define (checker/check-stmt checker ret-typ stmt)
  (match stmt
    ([CStmt/LocalVarDef typ name init-expr]
     (checker/add-rule checker (rule/same-type typ (rule/synthesis init-expr)))
     (checker/add-rule checker (rule/bind name typ)))
    ([CStmt/Assign name expr]
     (checker/add-rule checker (rule/same-type (rule/synthesis (CExpr/ID name)) (rule/synthesis expr))))
    ([CStmt/Return expr]
     (checker/add-rule checker (rule/same-type ret-typ (rule/synthesis expr))))))