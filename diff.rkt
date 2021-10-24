#lang racket/base

(require (for-syntax racket/base
                     (prefix-in p: "prim.rkt")
                     racket/list
                     racket/set
                     racket/function
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     syntax/id-table
                     syntax/id-set
                     syntax/free-vars
                     ;; "apply.rkt"
                     "anf.rkt"
                     "reverse-transform.rkt"
                     ; "primitives.rkt"
                     "util.rkt")
         
         racket/unsafe/ops
         (prefix-in p: "prim.rkt")
         ;;"apply.rkt"
         (only-in "primitives.rkt"
                  proc-result*
                  proc-result?
                  gen-zero?))

(provide D+)

(module+ test
  (require racket/match
           rackunit))

(define (unknown-backprop op)
  (raise-arguments-error 'prim-definition
                         "Backpropagator unknown"
                         "op" op))

(define (strip-backprop p)
  (cond
    [(procedure? p)
     (λ xs (strip-backprop (apply p (map strip-backprop xs))))]
    [(proc-result? p) (p:primal p)]
    [else p]))


;; must be used while expanding (uses syntax-local-value)
;;
;; the primitive backprop definition takes a argument [box-adjoints:
;; identifier?], that is used by the definition to look for the global
;; backpropagator table (used to handle mutation)
(define-for-syntax (prim-definition box-adjoints prim)
  (define (make-unknown-backprop box-adjoints-unused)
    (with-syntax ([prim prim])
      ;#'(error 'prim)
      #'(λ xs
          (p:proc-result
           (apply (strip-backprop prim) xs)
           (λ (Aw)
             (if (gen-zero? Aw)
                 Aw
                 (unknown-backprop prim)))))))

  (define maybe-make-prim-backprop
    (syntax-property prim p:backprop-property-name))

  (define make-prim-backprop
    (or maybe-make-prim-backprop make-unknown-backprop))

  (make-prim-backprop box-adjoints))

(define-syntax (D+ stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ e)
     #:with e-anf
     (set!->set-box! (anf-normalize (local-expand #'e 'expression '())))

     #:with (let-values (((x-result-arg) e*)) x-result-final) (local-expand #'e-anf 'expression '())
     #:fail-unless (free-identifier=? #'x-result-arg #'x-result-final)
     "Result of anf-normalize had an unexpected form"

     #:with (De* (prim:id prim-intro:id) ...) (reverse-transform #'e*)

     #:with ((distinct-prim distinct-prim-intro) ...)
     (remove-duplicates (syntax-e #'((prim prim-intro) ...))
                        free-identifier=? #:key stx-car)

     #:with (prim-def ...)
     (stx-map (curry prim-definition #'box-adjoints) #'(distinct-prim ...))

     #'(let* ([box-adjoints (p:make-hasheq)]
              [distinct-prim-intro prim-def] ...
              [D+f De*])
         (λ xs
           (let ([primal+backprop (p:apply D+f xs)])
             (p:proc-result
              (p:primal primal+backprop)
              (λ Aw
                (p:coerce-zero
                 ;; drop terms from closed-over variables
                 (p:cdr (p:apply (p:backprop primal+backprop) Aw))
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

;; package structure and merge

;; additional backpropagators:
;;  - foldl/foldl0
;;  - math/array
;;  - hash tables (second derivative of)

;; put define-primitive in a separate module (this is part of the interface)

;; put prim definitions somewhere else (e.g. arith.rkt, base-prim.rkt, base.rkt?)

;; tips for writing backpropagators (doc page)
;;  - use scale and add instead of * and +, which will handle structured input and gen-zero
;;  - might be passed a gen-zero: make sure that this case is handled
;;    properly, including in e.g. tail position in a list, where it may
;;    mean null


;; general organization

;; match-let failure - expansion includes 'car' (from racket/base)
;;  - sensible way to 'rewrite' these (perhaps not - that was the original problem!)
;;    - caller can provide rewrite rules??
;;  - or abandon this idea (make it more self-containted)
;;  - list* without split-at
