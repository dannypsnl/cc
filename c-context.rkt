#lang racket

(require "c-def.rkt")
(require "c-type.rkt")

(provide context
         empty-context
         context/new-type
         context/lookup-type-id
         context/infer/type-of-expr
         context/check/expr-has-type)

(struct context
  [all-types type-name-to-id]
  #:mutable)

(define empty-context
  (lambda ()
    (context '() (make-hash '()))))

;;; context/new-type
; This function update context to remember type and update type-id
; type-definition default value is CBuiltin, stand for types like: "int", "bool"
(define (context/new-type ctx type-name [type-definition (CBuiltin)])
  ; 1. update all-types which stores all types by append type-definition into it
  (set-context-all-types! ctx (append (context-all-types ctx) (list type-definition)))
  ; 2. update type-name to type-id mapping
  (let ([type-id (length (context-all-types ctx))])
    (hash-set! (context-type-name-to-id ctx) type-name type-id)))

(define (context/lookup-type-id ctx type-name [check-struct #f])
  (let* ([type-id (hash-ref (context-type-name-to-id ctx) type-name (lambda () (raise (format "no type named ~a" type-name))))]
         [type-definition (list-ref (context-all-types ctx) (- type-id 1))])
    (cond
      ; 1. is struct but no modifier `struct`
      ([boolean=? (and (CStruct? type-definition) (not check-struct)) #t]
       (raise (format "type ~a is struct, must provide keyword: `struct`" type-name)))
      ; 1. is not struct but have modifier `struct`
      ([boolean=? (and (not (CStruct? type-definition)) check-struct) #t]
       (raise (format "type ~a is not a struct, keyword `struct` should be removed" type-name))))
    type-id))

(define (context/infer/type-of-expr ctx c-expr)
  (match c-expr
    ([CExpr/Int _] (context/lookup-type-id ctx "int"))
    ([CExpr/Bool _] (context/lookup-type-id ctx "bool"))
    ([CExpr/ID _] (raise "unimplement infer <CExpr/ID> type yet!"))
    ([CExpr/Binary _ left-expr _] (context/infer/type-of-expr ctx left-expr))))

(define (context/check/expr-has-type ctx c-expr type)
  (let ([type-of-expr (context/infer/type-of-expr ctx c-expr)])
    (if (= type-of-expr type)
       'ok
       (raise (format "type mismatched, expected: ~a but got: ~a"
                      (context-all-types ctx (- type 1))
                      (context-all-types ctx (- type-of-expr 1)))))))

(module+ test
  (require rackunit)

  (test-case
   "context/new-type would update all-types"
   (define test-ctx (empty-context))
   (context/new-type test-ctx "int")
   (context/new-type test-ctx "bool")
   (check-eq? (length (context-all-types test-ctx)) 2))

  (test-case
   "context/new-type would update type-id counting"
   (define test-ctx (empty-context))
   (context/new-type test-ctx "int")
   (define expect-type-id 1)
   (check-eq? (context/lookup-type-id test-ctx "int") expect-type-id))

  (test-case
   "context/new-type would update type-id counting -- second"
   (define test-ctx (empty-context))
   (context/new-type test-ctx "int")
   (context/new-type test-ctx "bool")
   (define expect-type-id 2)
   (check-eq? (context/lookup-type-id test-ctx "bool") expect-type-id))

  (test-case
   "structure type required keyword `struct` modifier"
   (define test-ctx (empty-context))
   (context/new-type test-ctx "Foo" (CStruct '()))
   (define expect-type-id 1)
   (check-eq? (context/lookup-type-id test-ctx "Foo" #t) expect-type-id))

  (test-case
   "infer type of expr"
   (define test-ctx (empty-context))
   (context/new-type test-ctx "int")
   (context/new-type test-ctx "bool")
   (check-eq? (context/infer/type-of-expr test-ctx (CExpr/Int 1)) 1)
   (check-eq? (context/infer/type-of-expr test-ctx (CExpr/Bool #t)) 2)
   (check-eq? (context/infer/type-of-expr test-ctx (CExpr/Binary + (CExpr/Int 1) (CExpr/Int 2))) 1))

  (test-case
   "check type of expr"
   (define test-ctx (empty-context))
   (context/new-type test-ctx "int")
   (context/new-type test-ctx "bool")
   (check-eq? (context/check/expr-has-type test-ctx (CExpr/Int 1) 1) 'ok)
   (check-eq? (context/check/expr-has-type test-ctx (CExpr/Bool #t) 2) 'ok)
   (check-eq? (context/check/expr-has-type test-ctx (CExpr/Binary + (CExpr/Int 1) (CExpr/Int 2)) 1) 'ok))

  )