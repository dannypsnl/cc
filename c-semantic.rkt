#lang racket

(require megaparsack)
(require "c-def.rkt"
         "c-context.rkt"
         "c-type.rkt")

(provide checker/new
         checker/check-ctop
         checker/report)

(struct checker
  [ctx
   rules
   errors]
  #:mutable)

(define (checker/new ctx)
  (checker ctx '() '()))
(define (checker/emit-error checker exn)
  (set-checker-errors! checker (append (checker-errors checker) (list exn))))

(define (checker/add-rule checker rule)
  (set-checker-rules! checker (append (checker-rules checker) (list rule))))
(define (checker/report checker)
  (map
   (λ (rule)
     (with-handlers
         ([error:semantic? (λ (err) (checker/emit-error checker err))])
       (context/execute-rule
        (checker-ctx checker)
        rule)))
   (checker-rules checker))
  (let ([errors (checker-errors checker)])
    (if (null? errors)
        'ok
        ((λ ()
           (map
            (λ (err)
              (displayln (error:semantic->string err)))
            errors
            )
           (raise "didn't pass semantic check"))))))

(define (checker/check-ctop checker boxed-ctop)
  (let ([loc (syntax-box-srcloc boxed-ctop)])
    (match (syntax-box-datum boxed-ctop)
      ([CGlobalVarDef typ name]
       (checker/add-rule checker (rule/bind loc name typ)))
      ([CStructDef _ _] 'ignore)
      ([CFuncDef ret-typ name param* stmt*]
       (for ([param param*])
         (match-let ([(list type-id name) param])
           (checker/add-rule checker (rule/bind loc name type-id))))
       (checker/add-rule checker (rule/push-env))
       (for ([stmt stmt*])
         (checker/check-stmt checker ret-typ stmt))
       (checker/add-rule checker (rule/pop-env))
       (checker/add-rule checker (rule/bind loc name (CFunction ret-typ (map (lambda (p) (car p)) param*))))))))

(define (checker/check-stmt checker ret-typ boxed-stmt)
  (let ([loc (syntax-box-srcloc boxed-stmt)])
    (match (syntax-box-datum boxed-stmt)
      ([CStmt/LocalVarDef typ name init-expr]
       (checker/add-rule checker (rule/same-type loc typ (rule/synthesis loc init-expr)))
       (checker/add-rule checker (rule/bind loc name typ)))
      ([CStmt/Assign name expr]
       (checker/add-rule checker (rule/same-type loc (rule/synthesis loc (CExpr/ID name)) (rule/synthesis loc expr))))
      ([CStmt/Return expr]
       (checker/add-rule checker (rule/same-type loc ret-typ (rule/synthesis loc expr))))
      ([CExpr/Call f arg*]
       (checker/add-rule checker (rule/apply loc (rule/synthesis loc f) arg*))))))
