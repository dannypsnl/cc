#lang racket

(require megaparsack)
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
   (λ (rule)
     (context/execute-rule
      (checker-ctx checker)
      rule))
   (checker-rules checker)))

(define (checker/check-ctop checker boxed-ctop)
  (match (syntax-box-datum boxed-ctop)
    ([CGlobalVarDef typ name]
     (checker/add-rule checker (rule/bind name typ)))
    ([CStructDef _ _] 'ignore)
    ([CFuncDef ret-typ name params stmts]
     (map
      (λ (param)
        (checker/add-rule checker (rule/bind (car (reverse param)) (car param))))
      params)
     (checker/add-rule checker (rule/push-env))
     (map
      (λ (stmt)
        (checker/check-stmt checker ret-typ stmt))
      stmts)
     (checker/add-rule checker (rule/pop-env)))))

(define (checker/check-stmt checker ret-typ boxed-stmt)
  (let ([loc (syntax-box-srcloc boxed-stmt)])
    (match (syntax-box-datum boxed-stmt)
      ([CStmt/LocalVarDef typ name init-expr]
       (checker/add-rule checker (rule/same-type loc typ (rule/synthesis loc init-expr)))
       (checker/add-rule checker (rule/bind name typ)))
      ([CStmt/Assign name expr]
       (checker/add-rule checker (rule/same-type loc (rule/synthesis loc (CExpr/ID name)) (rule/synthesis loc expr))))
      ([CStmt/Return expr]
       (checker/add-rule checker (rule/same-type loc ret-typ (rule/synthesis loc expr)))))))