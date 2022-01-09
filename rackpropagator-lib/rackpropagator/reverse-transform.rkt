#lang racket/base

(require (for-template racket/base
                       "builtins.rkt"
                       "prim-definition.rkt"
                       "sum-let.rkt")
         racket/list
         racket/dict
         racket/function
         racket/syntax
         racket/set
         syntax/id-table
         syntax/id-set
         syntax/parse
         syntax/stx
         "anf.rkt")

(provide reverse-transform)

(define (free-vars stx)
  (filter-not get-prim-definition (set->list (anf-free-vars stx))))

;; ----------------------------------------
;; Naming of variables in the reverse transform

(define (id-modifier [pre ""] [post ""])
  (let ([ids (make-free-id-table)])
    (λ (id)
      (let ([name (format-id #f "~a~a~a" pre id post)])
        (dict-ref! ids id (generate-temporary name))))))

(define backpropagator (id-modifier "<-" "-"))
(define sensitivity (id-modifier "^" "-"))
(define dummy (id-modifier "^" "-dummy-"))
(define reversed (id-modifier "" "*"))

;; ----------------------------------------
;; Syntax classes

(define-syntax-class id/backprop-ids
  (pattern x:id
           #:attr sensitivity (sensitivity #'x)
           #:attr backprop (backpropagator #'x)
           #:attr dummy (dummy #'x)
           #:attr reversed (reversed #'x)))

(define-syntax-class lambda-formals/backprop-ids
  (pattern (x:id/backprop-ids ...)
           #:attr (vars 1) (syntax->list #'(x ...))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ...))
           #:attr (sensitivity-result 1) (syntax->list #'(x.sensitivity ... null))
           #:attr reversed #'(x.reversed ...)
           #:attr (reversed-result 1) (syntax->list #'(x.reversed ... null)))
  (pattern xs:id/backprop-ids
           #:attr (vars 1) (syntax->list #'(xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'(xs.sensitivity))
           #:attr reversed #'xs.reversed
           #:attr (reversed-result 1) (syntax->list #'(xs.reversed)))
  (pattern (x:id/backprop-ids ...+ . xs:id/backprop-ids)
           #:attr (vars 1) (syntax->list #'(x ... xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr reversed #'(x.reversed ... . xs.reversed)
           #:attr (reversed-result 1) (syntax->list #'(x.reversed ... xs.reversed))))

(define-syntax-class nested-let-values
  #:literal-sets (kernel-literals)
  (pattern (let-values (B) body:nested-let-values)
           #:attr (bindings 1) (cons #'B (syntax-e #'(body.bindings ...)))
           #:attr result #'body.result)
  (pattern body:id
           #:attr (bindings 1) null
           #:attr result #'body))

;; ----------------------------------------


;; Note: takes a 'let-values' style binding, and produces a binding
;; form for destructuring-sum-let* (sheds one level of parens around
;; the id)
(define (ϕ b bound-ids)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (kernel-literals)
    [((lhs) c)
     (list #'(lhs.reversed c))]

    [((lhs) x)
     (define prims (filter get-prim-definition (list #'x)))
     (cons #'(lhs.reversed x.reversed)
           prims)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #:with transformed-expr (reverse-transform #'(#%plain-lambda formals M) bound-ids)
     (list #'(lhs.reversed transformed-expr))]

    [((lhs) (#%plain-app x0 xs ...))
     (define prims (filter get-prim-definition (syntax-e #'(x0 xs ...))))
     (cons #'((proc-result lhs.reversed lhs.backprop) (x0.reversed xs.reversed ...))
           prims)]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     (define prims (filter get-prim-definition (list #'x-test #'x-true #'x-false)))
     (cons #'((proc-result lhs.reversed lhs.backprop)
              (if x-test.reversed (x-true.reversed) (x-false.reversed)))
           prims)]))

(define (ρ b box-sensitivities)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (kernel-literals)
    [((lhs) c)
     #'(() lhs.sensitivity)]

    [((lhs) x)
     #'(x.sensitivity lhs.sensitivity)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #'((x-free.sensitivity ...) lhs.sensitivity)]

    [((lhs) (#%plain-app x0 xs ...))
     #'((x0.sensitivity xs.sensitivity ...)
        (lhs.backprop lhs.sensitivity box-sensitivities))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #'((x-true.sensitivity x-false.sensitivity)
        (let ([b (car (lhs.backprop lhs.sensitivity box-sensitivities))])
          (if x-test.reversed
              (list b (gen-zero))
              (list (gen-zero) b))))]))

(define (reverse-transform f [bound-ids '()])
  (syntax-parse f
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [formals lambda-formals/backprop-ids])
    #:literal-sets (kernel-literals)

    [prim:id
     #:attr prim* (get-prim-definition #'prim)
     (if (attribute prim*)
         #'prim*
         #'(unknown-transform prim 'prim))]

    [{~and lam (#%plain-lambda formals body:nested-let-values)}
     #:with (B ...) #'(body.bindings ...)
     #:with (x ...) #'(B.x ...)
     #:with result:id/backprop-ids #'body.result
     #:with (x-free ...) (free-vars #'lam)

     #:with (x-unknown ...)
     (set->list
      (set-subtract (immutable-free-id-set (syntax-e #'(x-free ...)))
                    (immutable-free-id-set bound-ids)))

     ;; add unknown to bound ids, so their reverse transform is only
     ;; included in the outermost lambda
     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(formals.vars ...))
                                      (syntax->list #'(x ...))
                                      (syntax->list #'(x-unknown ...))))]

     #:with ((primal-bindings prim ...) ...)
     (map (curryr ϕ bound-ids*) (syntax-e #'(B ...)))

     #:with (backprop-bindings ...)
     (map (curryr ρ #'box-sensitivities) (reverse (syntax-e #'(B ...))))

     #:with (prim-dedup:id/backprop-ids ...)
     (remove-duplicates (syntax-e #'(prim ... ...)) free-identifier=?)

     #:with (prim-def ...)
     (map get-prim-definition (syntax-e #'(prim-dedup ...)))

     #'(λ formals.reversed
         (let ([x-unknown.reversed (unknown-transform x-unknown 'x-unknown)] ...
               [prim-dedup.reversed prim-def] ...)
          (destructuring-sum-let* (primal-bindings ...)
            (proc-result
             result.reversed
             (λ (result.dummy box-sensitivities)
               (destructuring-sum-let*
                   ([result.sensitivity result.dummy]
                    [x-free.sensitivity (gen-zero)] ...
                    [formals.sensitivity-vars (gen-zero)] ...
                    [x.sensitivity (gen-zero)] ...
                    backprop-bindings ...)
                   (coerce-zero
                    (list* (list x-free.sensitivity ...)
                           formals.sensitivity-result ...)
                    (list* (list x-free.reversed ...)
                           formals.reversed-result ...))))))))]))
