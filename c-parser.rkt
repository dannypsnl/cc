#lang racket

(require data/monad data/applicative)
(require megaparsack megaparsack/text)

(require "c-def.rkt")

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
(define type/p identifier/p)

(define global-var-def/p
  (do [typ <- type/p]
      [name <- identifier/p]
      (char/p #\;)
      (pure (CGlobalVarDef typ name))))

(define struct-field/p
  (do [field <- (list/p type/p identifier/p)]
      (char/p #\;)
      (lexeme/p)
      (pure field)))
(define struct-def/p
  (do (keyword/p "struct")
      [name <- identifier/p]
      (char/p #\{)
      (lexeme/p)
      [fields <- (many/p struct-field/p)]
      (lexeme/p)
      (char/p #\})
      (pure (CStructDef name fields))))

(module+ test
  (require rackunit)
  (require data/either)
  (require megaparsack megaparsack/text)

  (define (t-parse parser input-string)
    (parse-string parser input-string))

  (check-equal? (t-parse (keyword/p "abc") "abc") (success "abc"))
  (check-equal? (t-parse identifier/p "a") (success "a"))
  (check-equal? (t-parse global-var-def/p "int i;")
    (success (CGlobalVarDef "int" "i")))
  (check-equal? (t-parse struct-def/p "struct Foo {}")
    (success (CStructDef "Foo" '())))
  (check-equal? (t-parse struct-def/p "struct Foo { int i; }")
    (success
      (CStructDef "Foo"
        (list
          (list "int" "i")))))
  (check-equal? (t-parse struct-def/p "struct Foo { int i; int j; }")
    (success
      (CStructDef "Foo"
        (list
          (list "int" "i")
          (list "int" "j")))))
  )
