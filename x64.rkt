#lang racket

(provide emit-to
         x64->string
         x64/reg
         x64/int
         x64/block
         x64/push
         x64/pop
         x64/mov
         x64/ret)

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
(struct x64/int [v])
(struct x64/push [bits reg])
(struct x64/pop [bits reg])
(struct x64/mov [bits dest src])
(struct x64/ret [bits])

(define (bits->suffix bits)
  (match bits
    (32 "l")
    (64 "q")))

(define (x64->string any-x64-v)
  (match any-x64-v
    ([x64/block tag insts]
     (string-append*
      (format "_~a:~n" tag)
      (map
       (lambda (v) (x64->string v))
       insts)))
    ([x64/push bits reg]
     (format "\tpush~a ~a~n"
             (bits->suffix bits)
             (x64->string reg)))
    ([x64/pop bits reg]
     (format "\tpop~a ~a~n"
             (bits->suffix bits)
             (x64->string reg)))
    ([x64/mov bits dest src]
     (format "\tmov~a ~a ~a~n"
             (bits->suffix bits)
             (x64->string dest)
             (x64->string src)))
    ([x64/ret bits]
     (format "\tret~a~n"
             (bits->suffix bits)))
    ([x64/internal/reg name shift]
     (if (= shift 0)
         (format "%~a" name)
         (format "~a(%~a)" shift name)))
    ([x64/int v] (format "$~a" v))))
