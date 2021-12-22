#lang racket/base

(require "apply.rkt"
         "builtins.rkt"
         "prim-definition.rkt"
         (for-syntax (except-in racket/base apply)
                     syntax/parse
                     "anf.rkt"
                     "apply.rkt"
                     "builtins.rkt"
                     "reverse-transform.rkt"))

(provide lift/D+
         D+
         grad
         (rename-out [grad ∇])
         define/D+)

(define-syntax lift/D+
  (syntax-parser
    ;; [(_ prim:id) (reverse-transform #'prim)]

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
      (anf-expand-expression #'(λ xs (apply expr xs)))))]))

(define-syntax D+
  (syntax-parser
    [(_ expr)
     #'(λ xs
         (let* ([D+f (lift/D+ expr)]
                [primal+backprop (apply D+f xs)]
                [Abox (make-hasheq)])
             (proc-result
              (primal primal+backprop)
              (λ (Aw)
                (coerce-zero
                 ;; drop terms from closed-over variables
                 (cdr ((backprop primal+backprop) Aw Abox))
                 xs)))))]))

(define-syntax grad/sensitivity
  (syntax-parser
    [(_ expr result-sensitivity)
     #'(λ xs
         (let* ([D+f (lift/D+ expr)]
                [<-f (backprop (apply D+f xs))]
                [Abox (make-hasheq)])
             (coerce-zero (cdr (<-f result-sensitivity Abox)) xs)))]))

(define-syntax grad
  (syntax-parser
    [(_ expr) #'(grad/sensitivity expr 1.0)]
    [(_ expr result-sensitivity) #'(grad/sensitivity expr result-sensitivity)]))

(define-syntax (define/D+ stx)
  (syntax-parse stx
    [(_ (f xs ...) body ...)
     #:with (xs* ...) (generate-temporaries #'(xs ...))
     #'(begin
         (define (f xs ...) body ...)
         (register-primitive!
          f
          (lift/D+ (λ (xs* ...)
                     (define (f xs ...) body ...)
                     (f xs* ...)))))]

    [(_ (f xs ... . xn) body ...)
     #:with (xs* ... xn*) (generate-temporaries #'(xs ... xn))
     #'(begin
         (define (f xs ... . xn) body ...)
         (register-primitive!
          f
          (lift/D+ (λ (xs* ... . xn*)
                     (define (f xs ... . xn) body ...)
                     (apply f xs* ... xn*)))))]

    [(_ f expr)
     #'(begin
         (define f expr)
         (register-primitive! f (lift/D+ (λ xs (apply expr xs)))))]))
