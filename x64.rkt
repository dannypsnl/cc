#lang racket

(provide x64/file x64/file->dump add-func add-global-var
         ; global variable
         x64/global-ref x64/global-var
         ; func
         emit-to x64/block
         x64->string x64/expr->bits
         x64/reg x64/int
         x64/push x64/pop
         x64/mov x64/ret x64/call
         ; integer arithmetic
         x64/add x64/sub x64/imul x64/idiv x64/cltd)

(struct x64/file
  [functions
   global-vars]
  #:transparent
  #:mutable)
(define (add-func file fn)
  (set-x64/file-functions! file (append (x64/file-functions file) (list fn))))
(define (add-global-var file gv)
  (set-x64/file-global-vars! file (append (x64/file-global-vars file) (list gv))))
(define (x64/file->dump file)
  (map
   (λ (f)
     (printf (x64->string f)))
   (x64/file-functions file))
  (map
   (λ (g)
     (printf (x64->string g)))
   (x64/file-global-vars file)))

(struct x64/global-var [name] #:transparent)
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
(struct x64/global-ref [name])
(struct x64/int [bits v])
(struct x64/push [bits reg])
(struct x64/pop [bits reg])
(struct x64/mov [bits src dest])
(struct x64/ret [bits])
(struct x64/call [bits f])
(struct x64/add [bits v loc])
(struct x64/sub [bits v loc])
(struct x64/imul [bits v loc])
(struct x64/idiv [bits v])
(struct x64/cltd [])

(define (x64/expr->bits expr)
  (match expr
    [(x64/int bits _) bits]
    [(x64/internal/reg _ _) 32]
    [(x64/add bits _ _) bits]
    [(x64/sub bits _ _) bits]
    [(x64/global-ref _) 32]))

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
       (λ (v) (x64->string v))
       insts)))
    ([x64/push bits reg]
     (format "\tpush~a ~a~n"
             (bits->suffix bits)
             (x64->string reg)))
    ([x64/pop bits reg]
     (format "\tpop~a ~a~n"
             (bits->suffix bits)
             (x64->string reg)))
    ([x64/mov bits src dest]
     (format "\tmov~a ~a, ~a~n"
             (bits->suffix bits)
             (x64->string src)
             (x64->string dest)))
    ([x64/ret bits]
     (format "\tret~a~n"
             (bits->suffix bits)))
    [(x64/call bits f)
     (format "\tcall~a ~a~n"
             (bits->suffix bits)
             f)]
    ([x64/add bits v loc]
     (format "\tadd~a ~a, ~a~n"
             (bits->suffix bits)
             (x64->string v)
             (x64->string loc)))
    ([x64/sub bits v loc]
     (format "\tsub~a ~a, ~a~n"
             (bits->suffix bits)
             (x64->string v)
             (x64->string loc)))
    ([x64/imul bits v loc]
     (format "\timul~a ~a, ~a~n"
             (bits->suffix bits)
             (x64->string v)
             (x64->string loc)))
    ([x64/idiv bits v]
     (format "\tidiv~a ~a~n"
             (bits->suffix bits)
             (x64->string v)))
    ([x64/cltd] "\tcltd~n")
    ([x64/internal/reg name shift]
     (if (= shift 0)
         (format "%~a" name)
         (format "~a(%~a)" shift name)))
    ([x64/int _ v] (format "$~a" v))
    ([x64/global-ref name] (format "_~a@GOTPCREL(%rip)" name))
    ([x64/global-var name] (format "\t.comm _~a,4,2~n" name))))
