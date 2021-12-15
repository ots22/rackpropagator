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

    ;; When expr is a lambda expression anf-outer-binding succeeds (non-#f)
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
