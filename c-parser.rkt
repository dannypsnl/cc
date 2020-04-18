#lang racket

(require data/monad data/applicative)
(require megaparsack megaparsack/text)

;;; for syntax definition
(require "c-def.rkt")
;;; for type definition
(require "c-type.rkt")
;;; for context
(require "c-context.rkt")

(define lexeme/p
  ;;; lexeme would take at least one space or do nothing
  (do (or/p (many+/p space/p) void/p)
      (pure (lambda () 'lexeme))))
(define (keyword/p keyword)
  (do (string/p keyword)
      (lexeme/p)
      (pure keyword)))
(define identifier/p
  (do [id <- (many+/p letter/p)]
      (lexeme/p)
      (pure (list->string id))))
(define (type/p ctx)
  (do [check-struct <- (or/p (keyword/p "struct") void/p)]
      [typ <- identifier/p]
      (pure ((lambda ()
        (context/lookup-type-id ctx typ (eqv? check-struct "struct")))))))

(define (global-var-def/p ctx)
  (do [typ <- (type/p ctx)]
      [name <- identifier/p]
      (char/p #\;)
      (pure (CGlobalVarDef typ name))))

(define (struct-field/p ctx)
  (do [field <- (list/p (type/p ctx) identifier/p)]
      (char/p #\;)
      (lexeme/p)
      (pure field)))
(define (struct-def/p ctx)
  (do (keyword/p "struct")
      [name <- identifier/p]
      (char/p #\{)
      (lexeme/p)
      [fields <- (many/p (struct-field/p ctx))]
      (lexeme/p)
      (char/p #\})
      (pure ((lambda ()
               (context/new-type ctx name (CStruct fields))
               (CStructDef name fields))))))

(define (func-arg/p ctx)
  (list/p (type/p ctx) identifier/p))
(define func-def/p
  (lambda (ctx)
    (do [ret-typ <- (type/p ctx)]
      [name <- identifier/p]
      (char/p #\()
      (lexeme/p)
      [params <- (many/p (func-arg/p ctx) #:sep (list/p (char/p #\,) lexeme/p))]
      (lexeme/p)
      (char/p #\))
      (lexeme/p)
      (char/p #\{)
      (lexeme/p)
      ;;; TODO: parse statement+
      (lexeme/p)
      (char/p #\})
      (pure (CFuncDef ret-typ name params '())))))

(module+ test
  (require rackunit)
  (require data/either)
  (require megaparsack megaparsack/text)

  (define (t-parse parser input-string)
    (parse-string parser input-string))
  (define test-ctx (empty-context))
  (context/new-type test-ctx "int")

  (check-equal? (t-parse (keyword/p "abc") "abc") (success "abc"))
  (check-equal? (t-parse identifier/p "a") (success "a"))
  (check-equal? (t-parse (global-var-def/p test-ctx) "int i;")
    (success (CGlobalVarDef 1 "i")))
  (check-equal? (t-parse (struct-def/p test-ctx) "struct Foo {}")
    (success (CStructDef "Foo" '())))
 (check-equal? (t-parse (global-var-def/p test-ctx) "struct Foo foo;")
    (success
      (CGlobalVarDef 2 "foo")))
  (check-equal? (t-parse (struct-def/p test-ctx) "struct Foo { int i; }")
    (success
      (CStructDef "Foo"
        (list
          (list 1 "i")))))
  (check-equal? (t-parse (struct-def/p test-ctx) "struct Foo { int i; int j; }")
    (success
      (CStructDef "Foo"
        (list
          (list 1 "i")
          (list 1 "j")))))
  (check-equal? (t-parse (func-def/p test-ctx) "int foo() {}")
    (success
      (CFuncDef 1 "foo" '() '())))
  (check-equal? (t-parse (func-def/p test-ctx) "int foo(int x) {}")
    (success
      (CFuncDef 1 "foo" (list (list 1 "x"))
        '())))
  (check-equal? (t-parse (func-def/p test-ctx) "int foo(int x, int y) {}")
    (success
      (CFuncDef 1 "foo" (list (list 1 "x") (list 1 "y"))
        '())))
  )
