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
  (do [id <- (many/p letter/p)]
      (lexeme/p)
      (pure (list->string id))))
(define type/p identifier/p)

(define global-var-def/p
  (do [typ <- type/p]
      [name <- identifier/p]
      (char/p #\;)
      (pure (CGlobalVarDef typ name))))

(define struct-def/p
  (do (keyword/p "struct")
      [name <- identifier/p]
      (char/p #\{)
      ;;; TODO: parse field
      (char/p #\})
      (pure 'struct)))

(module+ test
  (require rackunit)
  (require megaparsack megaparsack/text)

  (define (t-parse parser input-string)
    (parse-result! (parse-string parser input-string)))

  (check-equal? (t-parse (keyword/p "abc") "abc") "abc")
  (check-equal? (t-parse identifier/p "a") "a")
  (check-equal? (t-parse global-var-def/p "int i;")
    (CGlobalVarDef "int" "i"))
  (check-equal? (t-parse struct-def/p "struct Foo {}")
    'struct)
  )
