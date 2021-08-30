#lang racket/base

(require (for-template racket/base
                       "anf.rkt"
                       (only-in "util.rkt"
                                destructuring-sum-letrec
                                destructuring-sum-lazy-letrec)
                       "primitives.rkt")
         (for-syntax racket/base
                     "anf.rkt")
         racket/list
         racket/dict
         racket/function
         racket/syntax
         racket/set
         syntax/parse
         syntax/stx
         syntax/id-table
         syntax/id-set
         syntax/free-vars
         "anf.rkt")

(provide reverse-transform)

(module+ test (require rackunit))

(define (id-modifier [pre ""] [post ""])
  (let ([ids (make-free-id-table)])
    (λ (id)
      (let ([name (format-id #f "~a~a~a" pre id post)])
        (dict-ref! ids id (generate-temporary name))))))

(define backpropagator (id-modifier "<-" "-"))
(define sensitivity (id-modifier "^" "-"))
(define introduction (id-modifier "D+" "-"))
(define tagged (id-modifier "" "*"))
(define dummy (id-modifier "$" ""))

(define-syntax-class id/backprop-ids
  (pattern x:id
           #:attr sensitivity (sensitivity #'x)
           #:attr backprop (backpropagator #'x)
           #:attr intro (introduction #'x)
           #:attr tagged (tagged #'x)
           #:attr dummy (dummy #'x)))

(define-syntax-class anf-let-final/backprop-ids
  #:literal-sets (anf-literals)
  #:local-conventions ([#rx"^x" id/backprop-ids])
  (pattern x
           #:attr formals #'(x)
           #:attr tagged #'x.tagged)
  (pattern (#%plain-app values xs ...)
           #:attr formals #'(xs ...)
           #:attr tagged #'(#%plain-app values xs.tagged ...))
  (pattern (#%plain-app anf-apply values xs ... xn)
           #:attr formals #'(xs ... . xn)
           #:attr tagged #'(#%plain-app anf-apply values xs.tagged ... xn.tagged)))

;; the sensitivities are thunks (defined with destructuring-sum-lazy-letrec)
;; sensitivity-result produces a result of the appropriate shape, forcing each thunk
(define-syntax-class lambda-formals/backprop-ids
  (pattern (x:id/backprop-ids ...)
           #:attr (vars 1) (syntax->list #'(x ...))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ...))
           #:attr (sensitivity-result 1) (syntax->list #'((x.sensitivity) ... null))
           #:attr tagged #'(x.tagged ...)
           #:attr dummy #'(x.dummy ...)
           #:attr (dummy-vars 1) (syntax->list #'(x.dummy ...)))
  (pattern xs:id/backprop-ids
           #:attr (vars 1) (syntax->list #'(xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'((xs.sensitivity)))
           #:attr tagged #'xs.tagged
           #:attr dummy #'xs.dummy
           #:attr (dummy-vars 1) (syntax->list #'(xs.dummy)))
  (pattern (x:id/backprop-ids ...+ . xs:id/backprop-ids)
           #:attr (vars 1) (syntax->list #'(x ... xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'((x.sensitivity) ... (xs.sensitivity)))
           #:attr tagged #'(x.tagged ... . xs.tagged)
           #:attr dummy #'(x.dummy ... . xs.dummy)
           #:attr (dummy-vars 1) (syntax->list #'(x.dummy ... xs.dummy))))

;; Note: takes a 'let-values' style binding, and produces a binding
;; form for sum-destructuring-letrec (sheds one level of parens around
;; the id)
(define (ϕ b bound-ids)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids]
                         [lhs0 id/backprop-ids])
    #:literal-sets (anf-literals)
    [((lhs) c)
     (list #'((lhs.tagged) c))]

    [((lhs) x)
     #:do [(define prims (if (member #'x bound-ids free-identifier=?)
                             '()
                             (list #'x)))]
     (cons #'((lhs.tagged) x.tagged)
           prims)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #:with (transformed-expr (prim _) ...)
            (reverse-transform #'(#%plain-lambda formals M) bound-ids)
     (cons #'(lhs.tagged transformed-expr)
           (syntax-e #'(prim ...)))]

    ;; If any of x0 xs ... are not in the list of bound ids, add to the list of ids
    ;; for which to introduce a primitive backpropagator definition
    [((lhs0 lhs ...) (#%plain-app x0 xs ...))
     #:do [(define prims
             (set->list
              (set-subtract (immutable-free-id-set (syntax-e #'(x0 xs ...)))
                            (immutable-free-id-set bound-ids))))]
     (cons #'((lhs0.backprop lhs0.tagged lhs.tagged ...) (x0.tagged xs.tagged ...))
           prims)]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     (list #'((lhs.backprop lhs.tagged)
              (if x-test.tagged (x-true.tagged) (x-false.tagged))))]
     ))

(define (ρ b)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs0 id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (anf-literals)
    [((lhs) c)
     #'(() (lhs.sensitivity))]

    [((lhs) x)
     #'(x.sensitivity (lhs.sensitivity))]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #'((x-free.sensitivity ...) (lhs.sensitivity))]

    [((lhs0 lhs ...) (#%plain-app x0 xs ...))
     #'((x0.sensitivity xs.sensitivity ...)
        (lhs0.backprop (lhs0.sensitivity) (lhs.sensitivity) ...))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #'((x-true.sensitivity x-false.sensitivity)
        (if x-test.tagged
            (list (car (lhs.backprop (lhs.sensitivity))) (gen-zero))
            (list (gen-zero) (car (lhs.backprop (lhs.sensitivity))))))]
    ;;
    ))

(define (reverse-transform f [bound-ids '()])
  (syntax-parse f
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [prim id/backprop-ids]
                         [formals lambda-formals/backprop-ids]
                         [result-formals lambda-formals/backprop-ids])
    #:literal-sets (anf-literals)

    [{~and lam (#%plain-lambda formals
                 (letrec-values {~and (B ...) orig-bindings}
                   result:anf-let-final/backprop-ids))}
     ;; #:when (andmap free-identifier=?
     ;;                (syntax->list #'(Bn.xs ...))
     ;;                (syntax->list #'(xns* ...)))
     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(formals.vars ...))
                                      (syntax->list #'(B.xs ... ...))))]
     ;; Note: free-vars returns only let/lambda bindings. These will be
     ;; a subset of bound-ids* that are used in the lambda body.
     #:with (x ...) #'(B.xs ... ...)
     #:with result-formals #'result.formals
     #:with (x-free ...) (free-vars #'lam)
     #:with ((primal-bindings prim ...) ...)
            (stx-map (curryr ϕ bound-ids*) #'orig-bindings)
     #:with (backprop-bindings ...) (map ρ (reverse (syntax-e #'orig-bindings)))
     (cons
      #'(λ formals.tagged
          (destructuring-sum-letrec (primal-bindings ...)
             (values (λ result-formals.dummy
                       (destructuring-sum-lazy-letrec
                        ([x-free.sensitivity (gen-zero)] ...
                         [formals.sensitivity-vars (gen-zero)] ...
                         [x.sensitivity (gen-zero)] ...
                         [result-formals.sensitivity-vars result-formals.dummy-vars] ...
                         backprop-bindings ...)
                        (list* (list (x-free.sensitivity) ...)
                               formals.sensitivity-result ...)))
                     result.tagged)))
      (syntax-e #'((prim prim.tagged) ... ...)))]

    ;; The cases below introduce potentially dangerous modifications
    ;; of the fully-expanded syntax: the introduced temporary tmp
    ;; cannot be returned by free-vars so this is in fact okay.
    ;;
    ;; All cases are then handled uniformly, with the first case
    ;;
    [(#%plain-lambda formals x)
     #:with tmp (generate-temporary)
     #:with lam* #'(#%plain-lambda formals
                     (letrec-values (((tmp) x))
                       tmp))
     (reverse-transform #'lam* bound-ids)]

    [(#%plain-lambda formals (#%plain-app values xs ...))
     #:with (tmps ...) (generate-temporaries #'(xs ...))
     #:with lam* #'(#%plain-lambda formals
                     (letrec-values (((tmps ...) (#%plain-app values xs ...)))
                       (#%plain-app values tmps ...)))
     (reverse-transform #'lam* bound-ids)]

    [(#%plain-lambda formals (#%plain-app anf-apply values xs ... xn))
     #:with (tmps ... tmpn) (generate-temporaries #'(xs ... xn))
     #:with lam* #'(#%plain-lambda formals
                     (letrec-values (((tmps) xs) ... ((tmpn) xn))
                       (#%plain-app anf-apply values tmps ... tmpn)))
     (reverse-transform #'lam* bound-ids)]


    ;; pattern to capture the last binding: not needed any more
    ;; [(#%plain-lambda formals
    ;;    (letrec-values (B ... Bn)
    ;;      {~or* {~and xn {~seq xns* ...}}
    ;;            (#%plain-app values . xns*)}))
    ;;  #:with tmp (generate-temporary)
    ;;  #:with lam* #'(#%plain-lambda formals
    ;;                  (letrec-values (B ... Bn ((tmp) xn*))
    ;;                    tmp))
    ;;  (reverse-transform #'lam* bound-ids)]



    ;;
    ))


(module+ test
  (check-not-exn
   (λ ()
     (reverse-transform
      #'(#%plain-lambda (x)
          (letrec-values (((result) (#%plain-app + x x)))
            (#%plain-app apply values result result)))))))



