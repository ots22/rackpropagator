#lang racket/base

(require "builtins.rkt"
         "prim-definition.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     "anf.rkt"
                     "builtins.rkt"
                     "reverse-transform.rkt")
         (for-template racket/base))

(provide lift/D+
         D+
         D
         grad
         (rename-out [grad ∇])
         grad1
         define/D+)

(define-syntax (lift/D+ stx)
  (syntax-parse stx
    [(_ prim:id)
     (reverse-transform (local-expand #'prim 'expression '()))]

    ;; When expr is a lambda expression, anf-outer-binding succeeds (non-#f)
    [(_ expr)
     #:attr expr* (anf-outer-binding (anf-expand-expression #'expr))
     #:when (attribute expr*)
     (reverse-transform #'expr*)]

    ;; If not an identifier or a lambda expression, assume that it is
    ;; a function and eta expand
    [(_ expr)
     (reverse-transform
      (anf-outer-binding
       (anf-expand-expression (syntax/loc stx (λ xs (apply expr xs))))))]))

(define-syntax-parse-rule (D+ expr)
  (λ xs
    (let* ([D+f (lift/D+ expr)]
           [primal+backprop (apply D+f xs)]
           [Abox (make-empty-hasheq)])
      (proc-result
       (primal primal+backprop)
       (λ (Aw)
         ;; drop terms from closed-over variables
         (cdr ((backprop primal+backprop) Aw Abox)))))))

(define-syntax-parse-rule (D expr)
  (λ xs
    (let* ([D+f (lift/D+ expr)]
           [primal+backprop (apply D+f xs)]
           [Abox (make-empty-hasheq)])
      (λ (Aw)
        ;; drop terms from closed-over variables
        (cdr ((backprop primal+backprop) Aw Abox))))))

(define-syntax-parse-rule (grad expr)
  (λ xs
    (let* ([D+f (lift/D+ expr)]
           [result (apply D+f xs)]
           [fxs (primal result)]
           [<-fxs (backprop result)]
           [Abox (make-empty-hasheq)])
      (hash-set! Abox fxs 1.0)
      (cdr (<-fxs 1.0 Abox)))))

(define-syntax-parse-rule (grad1 expr)
  (λ (x) (car ((grad expr) x))))

(define-syntax (define/D+ stx)
  (syntax-parse stx
    [(_ (f xs ...) body ...+)
     #:with (xs* ...) (generate-temporaries #'(xs ...))
     #:with def (syntax/loc stx (define (f xs ...) body ...))
     #:with def-wrapped (syntax/loc stx (λ (xs* ...)
                                          def
                                          (f xs* ...)))
     #'(begin
         def
         (register-primitive! f (lift/D+ def-wrapped)))]

    [(_ (f xs ... . xn) body ...+)
     #:with (xs* ... xn*) (generate-temporaries #'(xs ... xn))
     #:with def (syntax/loc stx (define (f xs ... . xn) body ...))
     #:with def-wrapped (syntax/loc stx
                          (λ (xs* ... . xn*)
                            def
                            (apply f xs* ... xn*)))
     #'(begin
         def
         (register-primitive! f (lift/D+ def-wrapped)))]

    [(_ f expr)
     #:with def (syntax/loc stx (define f expr))
     #:with def-wrapped (syntax/loc stx (λ xs (apply expr xs)))
     #'(begin
         def
         (register-primitive! f (lift/D+ def-wrapped)))]))
