#lang racket/base

(require racket/stxparam
         "apply.rkt"
         "builtins.rkt"
         "prim-definition.rkt"
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


(define (unknown-backprop op)
  (raise-arguments-error 'prim-definition
                         "Backpropagator unknown"
                         "op" op))

(define (strip-backprop p)
  (cond
    [(procedure? p)
     (λ xs (strip-backprop (apply p (map strip-backprop xs))))]
    [(proc-result? p) (primal p)]
    [else p]))

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

     ;; #:with non-prim-augmented #'(λ xs
     ;;                               (proc-result
     ;;                                (apply (strip-backprop other) xs)
     ;;                                (λ (Aw)
     ;;                                  (if (gen-zero? Aw)
     ;;                                      Aw
     ;;                                      (unknown-backprop 'other)))))

     #:with (prim-def ...) #'((prim-definition distinct-prim) ...)

     #'(let ([box-adjoints (make-hasheq)])
         (syntax-parameterize ([current-box-adjoints
                                (make-rename-transformer #'box-adjoints)]
                               ;; [current-non-prim-transform
                               ;;  (syntax-parser
                               ;;    [other
                               ;;     #'(λ xs
                               ;;         (proc-result
                               ;;          (apply (strip-backprop other) xs)
                               ;;          (λ (Aw)
                               ;;            (if (gen-zero? Aw)
                               ;;                Aw
                               ;;                (unknown-backprop 'other)))))])]
                               )
           (let ([distinct-prim-intro prim-def] ...)
             (let ([D+f De*])
               (λ xs
                 (let ([primal+backprop (apply D+f xs)])
                   (proc-result
                    (primal primal+backprop)
                    (λ Aw
                      (coerce-zero
                       ;; drop terms from closed-over variables
                       (cdr (apply (backprop primal+backprop) Aw))
                       xs)))))))))]))
