#lang racket/base

(require (for-template racket/base
                       (only-in "util.rkt"
                                destructuring-sum-let*)
                       "primitives.rkt")
         racket/list
         racket/dict
         racket/function
         racket/syntax
         racket/set
         syntax/parse
         syntax/stx
         syntax/id-table
         syntax/id-set
         "anf.rkt")

(provide reverse-transform)

(module+ test (require rackunit))

(define (id-modifier [pre ""] [post ""])
  (let ([ids (make-free-id-table)])
    (λ (id)
      (let ([name (format-id #f "~a~a~a" pre id post)])
        (dict-ref! ids id (generate-temporary name))))))

(define (free-vars stx)
  (set->list (anf-free-vars stx)))

(define backpropagator (id-modifier "<-" "-"))
(define sensitivity (id-modifier "^" "-"))
(define dummy (id-modifier "^" "-dummy-"))
(define tagged (id-modifier "" "*"))

(define-syntax-class id/backprop-ids
  (pattern x:id
           #:attr sensitivity (sensitivity #'x)
           #:attr backprop (backpropagator #'x)
           #:attr dummy (dummy #'x)
           #:attr tagged (tagged #'x)))

;; the sensitivities are thunks (defined with destructuring-sum-lazy-letrec)
;; sensitivity-result produces a result of the appropriate shape, forcing each thunk
(define-syntax-class lambda-formals/backprop-ids
  (pattern (x:id/backprop-ids ...)
           #:attr (vars 1) (syntax->list #'(x ...))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ...))
           #:attr (sensitivity-result 1) (syntax->list #'(x.sensitivity ... null))
           #:attr tagged #'(x.tagged ...))
  (pattern xs:id/backprop-ids
           #:attr (vars 1) (syntax->list #'(xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'(xs.sensitivity))
           #:attr tagged #'xs.tagged)
  (pattern (x:id/backprop-ids ...+ . xs:id/backprop-ids)
           #:attr (vars 1) (syntax->list #'(x ... xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr tagged #'(x.tagged ... . xs.tagged)))

(define-syntax-class nested-let-values
  #:literal-sets (kernel-literals)
  (pattern (let-values (B)
             body:nested-let-values)
           #:attr (bindings 1) (cons #'B (syntax-e #'(body.bindings ...)))
           #:attr result #'body.result)
  (pattern body:id
           #:attr (bindings 1) null
           #:attr result #'body))

;; Note: takes a 'let-values' style binding, and produces a binding
;; form for sum-destructuring-let (sheds one level of parens around
;; the id)
(define (ϕ b bound-ids)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (kernel-literals)
    [((lhs) c)
     (list #'(lhs.tagged c))]

    [((lhs) x)
     #:do [(define prims (if (member #'x bound-ids free-identifier=?)
                             '()
                             (list #'x)))]
     (cons #'(lhs.tagged x.tagged)
           prims)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #:with (transformed-expr (prim _) ...)
            (reverse-transform #'(#%plain-lambda formals M) bound-ids)
     (cons #'(lhs.tagged transformed-expr)
           (syntax-e #'(prim ...)))]

    ;; If any of x0 xs ... are not in the list of bound ids, add to the list of ids
    ;; for which to introduce a primitive backpropagator definition
    [((lhs) (#%plain-app x0 xs ...))
     #:do [(define prims
             (set->list
              (set-subtract (immutable-free-id-set (syntax-e #'(x0 xs ...)))
                            (immutable-free-id-set bound-ids))))]
     (cons #'((proc-result lhs.tagged lhs.backprop) (x0.tagged xs.tagged ...))
           prims)]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     (list #'((proc-result lhs.tagged lhs.backprop)
              (if x-test.tagged (x-true.tagged) (x-false.tagged))))]
    ;
    ))

(define (ρ b)
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
        (lhs.backprop lhs.sensitivity))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #'((x-true.sensitivity x-false.sensitivity)
        (let ([b (car (lhs.backprop lhs.sensitivity))])
          (if x-test.tagged
              (list b (gen-zero))
              (list (gen-zero) b))))]
    ;;
    ))

(define (reverse-transform f [bound-ids '()])
  (syntax-parse f
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [prim id/backprop-ids]
                         [formals lambda-formals/backprop-ids])
    #:literal-sets (kernel-literals)

    [{~and lam
           (#%plain-lambda formals
             body:nested-let-values)}

     #:with (B ...) #'(body.bindings ...)
     #:with (x ...) #'(B.x ...)
     #:with result:id/backprop-ids #'body.result

     ;; free-vars returns only let/lambda bindings. These will be a
     ;; subset of bound-ids* that are used in the lambda body
     #:with (x-free ...) (free-vars #'lam)

     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(formals.vars ...))
                                      (syntax->list #'(x ...))))]

     #:with ((primal-bindings prim ...) ...)
            (map (curryr ϕ bound-ids*) (syntax-e #'(B ...)))
     #:with (backprop-bindings ...)
            (map ρ (reverse (syntax-e #'(B ...))))

     (cons
      #'(λ formals.tagged
          (destructuring-sum-let* (primal-bindings ...)
            (proc-result
             result.tagged
             (λ (result.dummy)
               (destructuring-sum-let*
                   ([x-free.sensitivity (gen-zero)] ...
                    [formals.sensitivity-vars (gen-zero)] ...
                    [x.sensitivity (gen-zero)] ...
                    [result.sensitivity result.dummy]
                    backprop-bindings ...)
                 (list* (list x-free.sensitivity ...)
                        formals.sensitivity-result ...))))))
      (syntax-e #'((prim prim.tagged) ... ...)))]
    ;;
    ))

(module+ test
  (check-not-exn
   (λ ()
     (reverse-transform
      #'(#%plain-lambda (x)
          (let-values (((result) (#%plain-app + x x)))
            result))))))
