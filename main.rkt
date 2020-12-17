#lang racket/base

(require racket/file)
(require megaparsack megaparsack/text)
(require "c-parser.rkt"
         "c-context.rkt"
         "c-semantic.rkt"
         "c-ir.rkt"
         "x64.rkt")

(define (parse-file filename content ctx)
  (parse-result! (parse-string
                  (many/p (c-top/p ctx))
                  content
                  filename)))

(module+ main
  (require racket/cmdline)

  (define ctx (empty-context))
  (context/new-type ctx "void")
  (context/new-type ctx "int")
  (context/new-type ctx "bool")

  (define checker (checker/new ctx))

  (command-line
   #:program "cc"
   #:args (c-file)
   (define content (file->string c-file))
   (define prog (parse-file c-file content ctx))
   (map
    (λ (ctop)
      (checker/check-ctop checker ctop))
    prog)
   (checker/report checker)
   (let ([file (x64/file '() '())]
         [global-ctx (new-context)])
     (for ([ctop prog])
       (CTop->IR file global-ctx ctop))
     (x64/file->dump file))

   (printf "~n")))
