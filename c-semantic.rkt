#lang racket

(require megaparsack)
(require reporter)
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
(define (checker/pending-report checker report)
  (set-checker-errors! checker (append (checker-errors checker) (list report))))

(define (checker/add-rule checker rule)
  (set-checker-rules! checker (append (checker-rules checker) (list rule))))
(define (checker/report checker)
  (map
   (λ (rule)
     (with-handlers
         ([error:semantic? (λ (err) (checker/pending-report checker (error:semantic->report err)))])
       (context/execute-rule
        (checker-ctx checker)
        rule)))
   (checker-rules checker))
  (let ([report* (checker-errors checker)])
    (unless (null? report*)
      (for ([report report*])
        (print-text (report->text report)))
      (raise "didn't pass semantic check"))))

(define (checker/check-ctop checker boxed-ctop)
  (let ([loc (syntax-box-srcloc boxed-ctop)])
    (match (syntax-box-datum boxed-ctop)
      ([CGlobalVarDef typ name]
       (checker/add-rule checker (rule/bind loc name typ)))
      ([CStructDef _ _] 'ignore)
      ([CFuncDef ret-typ name param* stmt*]
       (checker/add-rule checker (rule/push-env))
       (for ([param param*])
         (match-let ([(list type-id name) param])
           (checker/add-rule checker (rule/bind loc name type-id))))
       (for ([stmt stmt*])
         (checker/check-stmt checker ret-typ stmt))
       (checker/add-rule checker (rule/pop-env))
       (checker/add-rule checker (rule/bind loc name (CFunction ret-typ (map (lambda (p) (car p)) param*))))))))

(define (checker/check-stmt checker ret-typ boxed-stmt)
  (let ([loc (syntax-box-srcloc boxed-stmt)])
    (match (syntax-box-datum boxed-stmt)
      ([CStmt/LocalVarDef typ name init-expr]
       (checker/add-rule checker (rule/same-type loc (cons (syntax-box-srcloc name) typ) (rule/synthesis init-expr)))
       (checker/add-rule checker (rule/bind loc name typ)))
      ([CStmt/Assign name expr]
       (define loc (syntax-box-srcloc name))
       (define n (syntax-box-datum name))
       (checker/add-rule checker (rule/same-type loc (rule/synthesis (syntax-box (CExpr/ID (syntax-box n loc)) loc)) (rule/synthesis expr))))
      ([CStmt/Return expr]
       (checker/add-rule checker (rule/same-type loc (cons loc ret-typ) (rule/synthesis expr))))
      ([CExpr/Call f arg*]
       (checker/add-rule checker (rule/apply loc (rule/synthesis (syntax-box (CExpr/ID (syntax-box (syntax-box-datum f) loc)) loc)) arg*))))))
