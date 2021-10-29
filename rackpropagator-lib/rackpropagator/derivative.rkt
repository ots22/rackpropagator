#lang racket/base

(require (for-syntax (except-in racket/base apply)
                     racket/list
                     racket/function
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     syntax/id-table
                     syntax/free-vars
                     "apply.rkt"
                     "anf.rkt"
                     "reverse-transform.rkt"
                     "builtins.rkt"
                     "util.rkt")
         racket/list
         racket/function
         "apply.rkt"
         "builtins.rkt"
         "primitives.rkt")

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


;; TODO

;; error messages (macros/syntax)

;; trick for defining additional primitives nicely/extensibly
;; perhaps both:
;;   - register-backprop
;;   - define/backprop (use reverse-transform/backprop,
;;     then 'register' using the trick)

;; cosmetics for D+:
;;   - explicit closure variables can be passed by user

;; 'lists' passed to backpropagators might have a tail of (gen-zero) (== null)
;;   - make sure this case is handled
;;   - other cases like this?

;; unbox0 needed? (test case?)

;; additional backpropagators:
;;  - foldl/foldl0
;;  - math/array
;;  - hash tables (second derivative of)
;;  - list* without split-at

;; put define-primitive in a separate module (this is part of the interface)

;; put prim definitions somewhere else (e.g. arith.rkt, base-prim.rkt, base.rkt?)

;; tips for writing backpropagators (doc page)
;;  - use scale and add instead of * and +, which will handle structured input and gen-zero
;;  - might be passed a gen-zero: make sure that this case is handled
;;    properly, including in e.g. tail position in a list, where it may
;;    mean null
