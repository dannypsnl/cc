#lang racket

(require "c-def.rkt"
         "c-type.rkt")

(provide context
         ; for context prepare
         empty-context
         context/new-type
         context/lookup-type-id
         ; rules
         context/execute-rule
         rule/push-env
         rule/pop-env
         rule/bind
         rule/synthesis
         rule/same-type
         ; semantic error
         error:semantic?
         error:semantic->string)

(struct context
  [;;; type-id fundamental
   all-types
   type-name-to-id
   ;;; type env
   env-ref]
  #:mutable)

(struct env
  [parent
   var-name-to-type]
  #:transparent
  #:mutable)

(struct error:semantic (loc))
(struct error:semantic:no-variable-named error:semantic (var-name))
(struct error:semantic:redefinition error:semantic (var-name))
(struct error:semantic:type-mismatched error:semantic (expect actual))
(define (error:semantic->string err)
  (match err
    ([error:semantic:no-variable-named loc var-name] (format "~a: no variable named: ~a" (srcloc->string loc) var-name))
    ([error:semantic:redefinition loc var-name] (format "~a: redefinition of `~a`" (srcloc->string loc) var-name))
    ([error:semantic:type-mismatched loc expect actual]
     (format "~a: type mismatched, expected: `~a` but got: `~a`"
             (srcloc->string loc)
             expect actual))))

(define (env/new [parent 'no-parent])
  (env parent (make-hash '())))
(define (env/lookup env loc var-name)
  (hash-ref (env-var-name-to-type env) var-name
            (λ ()
              (if (eqv? 'no-parent (env-parent env))
                  (raise (error:semantic:no-variable-named loc var-name))
                  (env/lookup (env-parent env) loc var-name)))))
(define (env/bind-var-name-with-type env loc var-name typ)
  (let ([env (env-var-name-to-type env)])
    (if (hash-has-key? env var-name)
        (raise (error:semantic:redefinition loc var-name))
        (hash-set! env var-name typ))))
(define (context/push-env ctx)
  (set-context-env-ref! ctx (env/new (context-env-ref ctx))))
(define (context/pop-env ctx)
  (set-context-env-ref! ctx (env-parent (context-env-ref ctx))))

(define (empty-context)
  (context '() (make-hash '()) (env/new)))

;;; context/new-type
; This function update context to remember type and update type-id
; type-definition default value is CBuiltin, stand for types like: "int", "bool"
(define (context/new-type ctx type-name [type-definition 'empty])
  (let ([type-definition (if (eqv? 'empty type-definition) (CBuiltin type-name) type-definition)])
    ; 1. update all-types which stores all types by append type-definition into it
    (set-context-all-types! ctx (append (context-all-types ctx) (list type-definition)))
    ; 2. update type-name to type-id mapping
    (let ([type-id (length (context-all-types ctx))])
      (hash-set! (context-type-name-to-id ctx) type-name type-id))))

(define (context/lookup-type-id ctx type-name
                                #:loc [loc ""]
                                #:struct? [check-struct #f])
  (let* ([type-id (hash-ref (context-type-name-to-id ctx) type-name (λ () (raise (format "~a no type named ~a" loc type-name))))]
         [type-definition (list-ref (context-all-types ctx) (- type-id 1))])
    (cond
      ; 1. is struct but no modifier `struct`
      ([boolean=? (and (CStruct? type-definition) (not check-struct)) #t]
       (raise (format "~a type ~a is struct, must provide keyword: `struct`" loc type-name)))
      ; 1. is not struct but have modifier `struct`
      ([boolean=? (and (not (CStruct? type-definition)) check-struct) #t]
       (raise (format "~a type ~a is not a struct, keyword `struct` should be removed" loc type-name))))
    type-id))

(define (context/infer/type-of-expr ctx loc c-expr)
  (match c-expr
    ([CExpr/Int _] (context/lookup-type-id ctx "int"))
    ([CExpr/Bool _] (context/lookup-type-id ctx "bool"))
    ([CExpr/ID var-name] (env/lookup (context-env-ref ctx) loc var-name))
    ([CExpr/Binary _ left-expr _] (context/infer/type-of-expr ctx loc left-expr))))

(struct rule/push-env [] #:transparent)
(struct rule/pop-env [] #:transparent)
(struct rule/bind [loc name typ] #:transparent)
(struct rule/synthesis [loc c-expr] #:transparent)
(struct rule/same-type
  [loc
   ;;; notice expected and actual are proper-rule!
   expected actual]
  #:transparent)

(define (context/execute-rule ctx rule)
  (match rule
    ([rule/push-env] (context/push-env ctx))
    ([rule/pop-env] (context/pop-env ctx))
    ([rule/bind loc name typ] (env/bind-var-name-with-type (context-env-ref ctx) loc name typ))
    ([rule/synthesis loc expr] (context/infer/type-of-expr ctx loc expr))
    ([rule/same-type loc expected actual]
     (let ([expect-typ (context/execute-rule ctx expected)]
           [actual-typ (context/execute-rule ctx actual)])
       (if (= expect-typ actual-typ)
           'ok
           (raise (error:semantic:type-mismatched loc
                                                  (type-definition->string (list-ref (context-all-types ctx) (- expect-typ 1)))
                                                  (type-definition->string (list-ref (context-all-types ctx) (- actual-typ 1))))))))
    ([var typ] typ)))

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
   (context/new-type test-ctx "Foo" (CStruct "Foo" '()))
   (define expect-type-id 1)
   (check-eq? (context/lookup-type-id test-ctx "Foo" #t) expect-type-id))

  (test-case
   "infer type of expr"
   (define test-ctx (empty-context))
   (context/new-type test-ctx "int")
   (context/new-type test-ctx "bool")
   (check-eq? (context/infer/type-of-expr test-ctx (srcloc 'test 1 1 1 1) (CExpr/Int 1)) 1)
   (check-eq? (context/infer/type-of-expr test-ctx (srcloc 'test 1 1 1 1) (CExpr/Bool #t)) 2)
   (check-eq? (context/infer/type-of-expr test-ctx (srcloc 'test 1 1 1 1) (CExpr/Binary + (CExpr/Int 1) (CExpr/Int 2))) 1))

  )
