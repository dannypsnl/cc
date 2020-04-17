#lang racket

(provide context
         empty-context
         context/new-type
         context/lookup-type-id)

(struct context
  [all-types type-name-to-id]
  #:mutable)

(define empty-context
  (lambda ()
    (context '() (make-hash '()))))

(define (context/new-type ctx type-name)
  (set-context-all-types! ctx (append (context-all-types ctx) (list type-name)))
  (let ([type-id (length (context-all-types ctx))])
    (hash-set! (context-type-name-to-id ctx) type-name type-id)))

(define (context/lookup-type-id ctx type-name)
  (hash-ref (context-type-name-to-id ctx) type-name (lambda () (raise (format "no type named ~a" type-name)))))

(module+ test
  (require rackunit)

  (test-case
    "context/new-type would update all-types"
    (define test-ctx (empty-context))
    (context/new-type test-ctx "int")
    (context/new-type test-ctx "bool")
    (check-eq? 2 (length (context-all-types test-ctx))))

  (test-case
    "context/new-type would update type-id counting"
    (define test-ctx (empty-context))
    (context/new-type test-ctx "int")
    (define expect-type-id 1)
    (check-eq? expect-type-id (context/lookup-type-id test-ctx "int")))

  (test-case
    "context/new-type would update type-id counting -- second"
    (define test-ctx (empty-context))
    (context/new-type test-ctx "int")
    (context/new-type test-ctx "bool")
    (define expect-type-id 2)
    (check-eq? expect-type-id (context/lookup-type-id test-ctx "bool")))

  )