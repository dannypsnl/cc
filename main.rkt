#lang racket/base

(require racket/file)
(require megaparsack megaparsack/text)
(require "c-parser.rkt")
(require "c-context.rkt")
(require "c-semantic.rkt")

(define (parse-file content ctx)
  (parse-result! (parse-string
                  (many/p (c-top/p ctx))
                  content)))

(module+ main
  (require racket/cmdline)

  (define ctx (empty-context))
  (context/new-type ctx "int")
  (context/new-type ctx "bool")

  (define checker (checker/new ctx))
  
  (command-line
   #:program "cc"
   #:args (c-file)
   (define content (file->string c-file))
   (map
    (lambda (ctop)
      (checker/check-ctop checker ctop))
    (parse-file content ctx))
   (checker/report checker)))
