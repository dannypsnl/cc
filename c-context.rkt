#lang racket

(require megaparsack)
(require reporter)
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
         rule/apply
         ; semantic error
         error:semantic?
         error:semantic->report)

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
(struct error:semantic:no-variable-named error:semantic (var))
(struct error:semantic:redefinition error:semantic (prev cur))
(struct error:semantic:type-mismatched error:semantic (expect-loc actual-loc expect actual))
(define (error:semantic->report err)
  (match err
    [(error:semantic:no-variable-named loc var)
     (report
       #:error-code "E0001"
       #:message "variable not found"
       #:target loc
       #:labels (list (label (syntax-box-srcloc var) "not found" #:color (color:red)))
       #:hint (format "no variable named: `~a`" (syntax-box-datum var)))]
    [(error:semantic:redefinition loc prev cur)
     (report
       #:error-code "E0002"
       #:message "redefine"
       #:target loc
       #:labels (list (label (syntax-box-srcloc prev) "previous definition" #:color (color:blue))
                      (label (syntax-box-srcloc cur) "current definition" #:color (color:red)))
       #:hint (format "redefintion of `~a`" (syntax-box-datum cur)))]
    [(error:semantic:type-mismatched loc expect-loc actual-loc expect actual)
     (report
       #:error-code "E0003"
       #:message "type mismatched"
       #:target loc
       #:labels (list (label expect-loc (format "expect: ~a" expect) #:color (color:blue))
                      (label actual-loc (format "actual: ~a" actual) #:color (color:red)))
       #:hint (format "expected: `~a` but got: `~a`" expect actual))]))

(define (env/new [parent 'no-parent])
  (env parent (make-hash '())))
(define (env/lookup env var)
  (define var-name (syntax-box-datum var))
  (hash-ref (env-var-name-to-type env) var-name
            (λ ()
              (if (eqv? 'no-parent (env-parent env))
                  (raise (error:semantic:no-variable-named (syntax-box-srcloc var) var))
                  (env/lookup (env-parent env) var)))))
(define (env/bind-var-name-with-type env loc var typ)
  (define var-name (syntax-box-datum var))
  (let* ([env (env-var-name-to-type env)]
         [defined? (hash-ref env var-name #f)])
    (if defined?
        (raise (error:semantic:redefinition loc (car defined?) var))
        (hash-set! env var-name (cons var typ)))))
(define (context/push-env ctx)
  (set-context-env-ref! ctx (env/new (context-env-ref ctx))))
(define (context/pop-env ctx)
  (set-context-env-ref! ctx (env-parent (context-env-ref ctx))))

(define (empty-context)
  (context '() (make-hash '()) (env/new)))

;;; context/new-type
; This function update context to remember type and update type-id
; type-definition default value is CBuiltin, stand for types like: "int", "bool"
(define (context/new-type ctx ty [type-definition 'empty])
  (define type-name (if (syntax-box? ty) (syntax-box-datum ty) ty))
  (let ([type-definition (if (eqv? 'empty type-definition) (CBuiltin type-name) type-definition)])
    ; 1. update all-types which stores all types by append type-definition into it
    (set-context-all-types! ctx (append (context-all-types ctx) (list type-definition)))
    ; 2. update type-name to type-id mapping
    (let ([type-id (length (context-all-types ctx))])
      (hash-set! (context-type-name-to-id ctx) type-name type-id))))

(define (context/lookup-type-id ctx ty
                                #:loc [loc ""]
                                #:struct? [check-struct #f])
  (define type-name (if (syntax-box? ty) (syntax-box-datum ty) ty))
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

(define (context/infer/type-of-expr ctx exp [loc? #f])
  (define loc (if loc? loc? (syntax-box-srcloc exp)))
  (define e (if (syntax-box? exp) (syntax-box-datum exp) exp))
  (define ty
   (match e
    [(CExpr/Int _) (context/lookup-type-id ctx "int")]
    [(CExpr/Bool _) (context/lookup-type-id ctx "bool")]
    [(CExpr/ID v) (cdr (env/lookup (context-env-ref ctx) v))]
    [(CExpr/Binary _ left-expr _) (cdr (context/infer/type-of-expr ctx left-expr loc))]
    [(CExpr/Call f _) (CFunction-ret (cdr (context/infer/type-of-expr ctx f loc)))]))
  (cons loc ty))

(struct rule/push-env [] #:transparent)
(struct rule/pop-env [] #:transparent)
(struct rule/bind [loc name typ] #:transparent)
(struct rule/synthesis [c-expr] #:transparent)
(struct rule/same-type
  [loc
   ;;; notice expected and actual are proper-rule!
   expected actual]
  #:transparent)
(struct rule/apply
  [loc
   synthe-function ;; rule
   arg*]
  #:transparent)

(define (context/execute-rule ctx rule)
  (match rule
    ([rule/push-env] (context/push-env ctx))
    ([rule/pop-env] (context/pop-env ctx))
    ([rule/bind loc name typ] (env/bind-var-name-with-type (context-env-ref ctx) loc name typ))
    ([rule/synthesis expr] (context/infer/type-of-expr ctx expr))
    ([rule/same-type loc expected actual]
     (let* ([expected (context/execute-rule ctx expected)]
            [actual (context/execute-rule ctx actual)]
            [expect-loc (car expected)]
            [actual-loc (car actual)]
            [expect-typ (cdr expected)]
            [actual-typ (cdr actual)])
       (unless (= expect-typ actual-typ)
           (raise (error:semantic:type-mismatched loc
                                                  expect-loc
                                                  actual-loc
                                                  (type-definition->string (list-ref (context-all-types ctx) (- expect-typ 1)))
                                                  (type-definition->string (list-ref (context-all-types ctx) (- actual-typ 1))))))))
    ([rule/apply loc synthe-function arg*]
     (let* ([func (context/execute-rule ctx synthe-function)]
            [func-ty (cdr func)]
            [func-loc (car func)])
       (for ([arg arg*]
             [expect-typ (CFunction-param* func-ty)])
         (define actual (context/execute-rule ctx (rule/synthesis arg)))
         (define actual-typ (cdr actual))
         (unless (= expect-typ actual-typ)
            (raise (error:semantic:type-mismatched loc
                                                   func-loc
                                                   (car actual)
                                                   (type-definition->string (list-ref (context-all-types ctx) (- expect-typ 1)))
                                                   (type-definition->string (list-ref (context-all-types ctx) (- actual-typ 1)))))))))
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
   (check-eq? (context/lookup-type-id test-ctx "Foo" #:struct? #t) expect-type-id)))
