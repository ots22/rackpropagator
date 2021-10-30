#lang racket/base

(require "apply.rkt"
         "builtins.rkt"
         "primitives.rkt"
         (for-syntax (except-in racket/base apply)
                     racket/list
                     racket/function
                     syntax/parse
                     syntax/stx
                     "anf.rkt"
                     "apply.rkt"
                     "builtins.rkt"
                     "reverse-transform.rkt"))

(provide D+)

(define-syntax (D+ stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ e)
     #:with e-anf
     (set!->set-box! (anf-normalize (local-expand #'e 'expression '())))

     #:with (let-values (((x-result-arg) e*)) x-result-final) #'e-anf
     #:fail-unless (free-identifier=? #'x-result-arg #'x-result-final)
     "Result of anf-normalize had an unexpected form"

     #:with (De* (prim:id prim-intro:id) ...) (reverse-transform #'e*)

     #:with ((distinct-prim distinct-prim-intro) ...)
     (remove-duplicates (syntax-e #'((prim prim-intro) ...))
                        free-identifier=? #:key stx-car)

     #:with (prim-def ...)
     (stx-map (curry prim-definition #'box-adjoints) #'(distinct-prim ...))

     #'(let* ([box-adjoints (make-hasheq)]
              [distinct-prim-intro prim-def] ...
              [D+f De*])
         (λ xs
           (let ([primal+backprop (apply D+f xs)])
             (proc-result
              (primal primal+backprop)
              (λ Aw
                (coerce-zero
                 ;; drop terms from closed-over variables
                 (cdr (apply (backprop primal+backprop) Aw))
                 xs))))))]))
