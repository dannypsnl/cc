#lang racket

(provide emit-to
         x64->string
         x64/reg
         x64/block
         x64/pushq
         x64/popq
         x64/movq
         x64/retq
         x64/movl)

(struct x64/block
  ; e.g.
  ; ```
  ; _add:
  ;     pushq %rbp
  ;     movq %rsp, %rbp
  ;     movq %rdi, -8(%rbp)
  ;     movl -8(%rbp), %eax
  ;     addl -4(%rbp), %eax
  ;     addl $1, %eax
  ;     popq %rbp
  ;     ret
  ; ```
  [tag instruction-list]
  #:transparent
  #:mutable)

(define (emit-to block inst)
  (set-x64/block-instruction-list! block
                                   (append (x64/block-instruction-list block) (list inst))))

(struct x64/internal/reg [name shift])
(define (x64/reg name [shift 0])
  (x64/internal/reg name shift))
(struct x64/pushq [reg])
(struct x64/popq [reg])
(struct x64/movq [dest src])
(struct x64/retq [])
(struct x64/movl [dest src])

(define (x64->string any-x64-v)
  (match any-x64-v
    ([x64/block tag insts]
     (string-append*
      (format "_~a:~n" tag)
      (map
       (lambda (v) (x64->string v))
       insts)))
    ([x64/pushq reg]
     (format "\tpushq ~a"
             (x64->string reg)))
    ([x64/popq reg] (x64->string reg))
    ([x64/movq dest src]
     (format "\tmovq ~a ~a"
             (x64->string dest)
             (x64->string src)))
    ([x64/retq] "\tretq")
    ([x64/movl dest src]
     (format "\tmovq ~a ~a"
             (x64->string dest)
             (x64->string src)))
    ([x64/internal/reg name shift]
     (if (= shift 0)
         (format "%~a" name)
         (format "~a(%~a)" shift name)))))