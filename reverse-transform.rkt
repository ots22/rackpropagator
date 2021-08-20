#lang racket/base

(require (for-template racket/base
                       (only-in "util.rkt"
                                destructuring-sum-letrec
                                destructuring-sum-lazy-letrec)
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

(define-syntax-class id/backprop-ids
  (pattern x:id
           #:attr sensitivity (sensitivity #'x)
           #:attr backprop (backpropagator #'x)
           #:attr intro (introduction #'x)
           #:attr tagged (tagged #'x)))


;; the sensitivities are thunks (defined with destructuring-sum-lazy-letrec)
;; sensitivity-result produces a result of the appropriate shape, forcing each thunk
(define-syntax-class lambda-formals/backprop-ids
  (pattern (x:id/backprop-ids ...)
           #:attr (vars 1) (syntax->list #'(x ...))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ...))
           #:attr (sensitivity-result 1) (syntax->list #'((x.sensitivity) ... null))
           #:attr tagged #'(x.tagged ...))
  (pattern xs:id/backprop-ids
           #:attr (vars 1) (syntax->list #'(xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'((xs.sensitivity)))
           #:attr tagged #'xs.tagged)
  (pattern (x:id/backprop-ids ...+ . xs:id/backprop-ids)
           #:attr (vars 1) (syntax->list #'(x ... xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'((x.sensitivity) ... (xs.sensitivity)))
           #:attr tagged #'(x.tagged ... . xs.tagged)))

;; Note: takes a 'let-values' style binding, and produces a binding
;; form for sum-destructuring-letrec (sheds one level of parens around
;; the id)
(define (ϕ b bound-ids)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (kernel-literals)
    [((lhs) (quote e))
     (list #'(lhs.tagged (quote e)))]

    [((lhs) (quote-syntax e))
     (list #'(lhs.tagged (quote-syntax e)))]

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
     (cons #'((lhs.tagged lhs.backprop) (x0.tagged xs.tagged ...))
           prims)]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     (list #'((lhs.tagged lhs.backprop)
              (if x-test.tagged (x-true.tagged) (x-false.tagged))))]
     ))

(define (ρ b)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (kernel-literals)
    [((lhs) (quote e))
     #'(() (lhs.sensitivity))]

    [((lhs) (quote-syntax e))
     #'(() (lhs.sensitivity))]

    [((lhs) x)
     #'(x.sensitivity (lhs.sensitivity))]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #'((x-free.sensitivity ...) (lhs.sensitivity))]

    [((lhs) (#%plain-app x0 xs ...))
     #'((x0.sensitivity xs.sensitivity ...)
        (lhs.backprop (lhs.sensitivity)))]

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
                         [prim id/backprop-ids])
    #:literal-sets (kernel-literals)

    ;; The first two cases below are potentially dangerous
    ;; modifications of fully-expanded syntax, but the introduced
    ;; temporary tmp cannot be returned by free-vars so this is in
    ;; fact okay.
    ;;
    ;; This is needed so all cases are handled uniformly below, as a
    ;; sequence of bindings

    [(#%plain-lambda formals x)
     #:with tmp (generate-temporary)
     #:with lam* #'(#%plain-lambda formals
                     (letrec-values (((tmp) x))
                       tmp))
     (reverse-transform #'lam* bound-ids)]

    [(#%plain-lambda formals
       (letrec-values (((x) B) ... ((xn) Bn))
         xn*))
     #:when (not (free-identifier=? #'xn #'xn*))
     #:with tmp (generate-temporary)
     #:with lam* #'(#%plain-lambda formals
                     (letrec-values (((x) B) ... ((xn) Bn) ((tmp) xn*))
                       tmp))
     (reverse-transform #'lam* bound-ids)]

    [{~and lam (#%plain-lambda formals:lambda-formals/backprop-ids
                 (letrec-values {~and (((x) B) ... ((xn) Bn))
                                      orig-bindings}
                   xn*))}
     #:when (free-identifier=? #'xn #'xn*)
     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(formals.vars ...))
                                      (syntax->list #'(x ... xn))))]
     ;; Note: free-vars returns only let/lambda bindings These will be
     ;; a subset of bound-ids* (bound-ids, in fact) that are used in
     ;; the lambda body
     #:with (x-free ...) (free-vars #'lam)
     #:with ((primal-bindings prim ...) ...)
            (stx-map (curryr ϕ bound-ids*) #'orig-bindings)
     #:with (backprop-bindings ...) (map ρ (reverse (syntax-e #'orig-bindings)))
     (cons
      #'(λ formals.tagged
          (destructuring-sum-letrec (primal-bindings ...)
            (list
             xn.tagged
             (λ (xn-sensitivity)
               (destructuring-sum-lazy-letrec
                   ([xn.sensitivity xn-sensitivity]
                    [x-free.sensitivity (gen-zero)] ...
                    [formals.sensitivity-vars (gen-zero)] ...
                    [x.sensitivity (gen-zero)] ...
                    backprop-bindings ...)
                 (list* (list (x-free.sensitivity) ...)
                        formals.sensitivity-result ...))))))
      (syntax-e #'((prim prim.tagged) ... ...)))]
    ;;
    ))


(module+ test
  (check-not-exn
   (λ ()
     (reverse-transform
      #'(#%plain-lambda (x)
          (letrec-values (((result) (#%plain-app + x x)))
            result))))))



