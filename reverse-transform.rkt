#lang racket/base

(require (for-template racket/base
                       "util.rkt"
                       "primitives.rkt"
                       "closure.rkt")
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
         "anf.rkt"
         "util.rkt"
         "closure.rkt"
         "primitives.rkt")

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

;; Note: takes a 'let-values' style binding, and produces a binding
;; form for sum-destructuring-letrec (sheds one level of parens around
;; the id)
(define (ϕ b bound-ids)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (kernel-literals)
    [((lhs) (quote c))
     (list #'(lhs.tagged (quote c)))]

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
     (cons #'(lhs.tagged (make-closure transformed-expr
                                       (list (zero x-free.tagged) ...)))
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
    [((lhs) (quote c))
     #'(() lhs.sensitivity)]

    [((lhs) x)
     #'(x.sensitivity lhs.sensitivity)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #'((x-free.sensitivity ...) lhs.sensitivity)]

    [((lhs) (#%plain-app x0 xs ...))
     #'((x0.sensitivity xs.sensitivity ...)
        (#%plain-app lhs.backprop lhs.sensitivity))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #'((x-true.sensitivity x-false.sensitivity)
        (if x-test.tagged
            (list (car (lhs.backprop lhs.sensitivity)) (zero x-false.tagged))
            (list (zero x-true.tagged) (car (lhs.backprop lhs.sensitivity)))))]
    ;;
    ))

(define (reverse-transform f [bound-ids '()])
  (syntax-parse f
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [prim id/backprop-ids])
    #:literal-sets (kernel-literals)
    [(#%plain-lambda formals x)
     ;; This is a potentially dangerous modification of
     ;; fully-expanded syntax, but the introduced temporary tmp cannot
     ;; be returned by free-vars so this is in fact okay.
     ;;
     ;; This is needed so all cases are handled uniformly below, as a
     ;; sequence of bindings
     #:with tmp (generate-temporary)
     #:with lam* #'(#%plain-lambda formals
                     (letrec-values (((tmp) x))
                       tmp))
     (reverse-transform #'lam* bound-ids)]

    [{~and lam (#%plain-lambda (x0 ...)
                 (letrec-values {~and (((x) B) ... ((xn) Bn))
                                      orig-bindings}
                   xn*))}
     #:when (free-identifier=? #'xn #'xn*)
     ;;#:with {~and orig-bindings (((x) e) ... ((xn) en))} (collect-bindings #'M)
     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(x0 ...))
                                      (syntax->list #'(x ... xn))))]
     ;; Note: free-vars returns only let/lambda bindings These will be
     ;; a subset of bound-ids* (bound-ids, in fact) that are used in
     ;; the lambda body
     #:with (x-free ...) (free-vars #'lam)
     #:with ((primal-bindings prim ...) ...)
            (stx-map (curryr ϕ bound-ids*) #'orig-bindings)
     #:with (backprop-bindings ...) (map ρ (reverse (syntax-e #'orig-bindings)))
     (cons
      #'(λ (x0.tagged ...)
          (destructuring-sum-letrec (primal-bindings ...)
            (list
             xn.tagged
             (λ (xn.sensitivity)
               (destructuring-sum-letrec ([x-free.sensitivity (zero x-free.tagged)] ...
                                          [x0.sensitivity (zero x0.tagged)] ...
                                          [x.sensitivity (zero x.tagged)] ...
                                          backprop-bindings ...)
                  (list (list x-free.sensitivity ...) x0.sensitivity ...))))))
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



