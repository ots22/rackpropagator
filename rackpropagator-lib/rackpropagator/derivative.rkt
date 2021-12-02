#lang racket/base

(require "apply.rkt"
         "primitives.rkt"
         (for-syntax (except-in racket/base apply)
                     syntax/parse
                     "anf.rkt"
                     "apply.rkt"
                     "builtins.rkt"
                     "reverse-transform.rkt"))

(provide lift/D+
         D+
         grad
         (rename-out [grad ∇]))

(define-syntax lift/D+
  (syntax-parser
    [(_ prim:id) (reverse-transform #'prim)]

    [(_ expr)
     #:attr expr* (anf-outer-binding (anf-expand-expression #'expr))
     #:when (attribute expr*)
     (reverse-transform #'expr*)]

    ;; If not an identifier or a lambda expression, assume that it is
    ;; a function and wrap in a lambda
    [(_ expr)
    (reverse-transform
     (anf-outer-binding
      (anf-expand-expression #'(λ xs (apply expr xs)))))]))

(define-syntax D+
  (syntax-parser
    [(_ expr)
     ;; figure out arity/formals of expr (which must be a function)
     #'(λ xs
         (let ([Abox (make-hasheq)]
               [D+f (lift/D+ expr)])
           (let ([primal+backprop (apply D+f xs)])
             (proc-result
              (primal primal+backprop)
              (λ (Aw)
                (coerce-zero
                 ;; drop terms from closed-over variables
                 (cdr ((backprop primal+backprop) Aw Abox))
                 xs))))))]))

(define-syntax directional-grad
  (syntax-parser
    [(_ expr dirn)
     ;; figure out arity/formals of expr (which must be a function)
     #'(λ xs
         (let ([Abox (make-hasheq)]
               [D+f (lift/D+ expr)])
           (let ([<-f (backprop (apply D+f xs))])
             (coerce-zero (cdr (<-f dirn Abox)) xs))))]))

(define-syntax grad
  (syntax-parser
    [(_ expr) #'(directional-grad expr 1.0)]
    [(_ expr dirn) #'(directional-grad expr dirn)]))


;; (define-syntax (define/backprop stx)
;;   (syntax-parse stx
;;     ;; TODO formals
;;     [(_ (f xs ...) body ...)
;;      #:with (xs* ...) (generate-temporaries #'(xs ...))
;;      #:with D*f (local-expand #'(D* (λ (xs* ...) (define (f xs ...) body ...) (f xs* ...)))
;;                               'expression
;;                               #f)
;;      #'(begin
;;          (define (f xs ...) body ...)
;;          (local-register-primitive! f D*f))]

;;     ;; [(_ (f . xs) body ...)
;;     ;;  #:with xs* (generate-temporaries #'xs)
;;     ;;  #:with D*f (local-expand #'(D* (λ xs* (define (f . xs) body ...) (apply f xs*)))
;;     ;;                           'expression
;;     ;;                           #f)
;;     ;;  #'(begin
;;     ;;      (define (f . xs) body ...)
;;     ;;      (local-register-primitive! f D*f))]

;;     ;; [(_ f body ...)
;;     ;;  #:with D*f (local-expand #'(D* (λ xs (define f body ...) (apply f xs)))
;;     ;;                           'expression
;;     ;;                           #f)
;;     ;;  #'(begin
;;     ;;      (define f body ...)
;;     ;;      (local-register-primitive! f D*f))]
;;     ))
