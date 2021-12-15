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
         syntax/parse
         syntax/stx
         syntax/id-table
         syntax/id-set
         syntax/parse
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
;; State: identifiers of introduced reverse-transformed primitives

(define prim->reverse-intro (make-free-id-table))

;; introduce-prim-def! identifier? syntax? -> identifier?
;;
;; Given p, an identifier representing a primitive, and p*-def,
;; syntax representing the definition of its reverse transformation,
;; lifts p*-def and returns the lifted identifier.  In addition,
;; p*-def is itself reverse transformed, and registered as the
;; primitive definition of the lifted identifier.
(define (introduce-prim-def! p p*-def)
  (let ([p*-lifted (syntax-local-lift-expression p*-def)])
    (dict-set! prim->reverse-intro p p*-lifted)
    (set-prim-definition! p*-lifted
                          (reverse-transform
                           (anf-outer-binding
                            (anf-expand-expression p*-def))))
    p*-lifted))

;; get-reversed : (listof identifier?) identifier? -> identifier?
;;
;; Look up the 'reversed' name of id: it is either a simple renaming
;; with reversed, or if id is a primitive, it is the name of the
;; introduced primitive definition.
(define (get-reversed id)
  (dict-ref prim->reverse-intro id
            (λ ()
              (cond
                [(get-prim-definition id) => (curry introduce-prim-def! id)]
                [else (reversed id)]))))

;; ----------------------------------------
;; Syntax classes

(define-syntax-class id/backprop-ids
  (pattern x:id
           #:attr sensitivity (sensitivity #'x)
           #:attr backprop (backpropagator #'x)
           #:attr dummy (dummy #'x)
           #:attr reversed (get-reversed #'x)))

(define-syntax-class lambda-formals/backprop-ids
  (pattern (x:id/backprop-ids ...)
           #:attr (vars 1) (syntax->list #'(x ...))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ...))
           #:attr (sensitivity-result 1) (syntax->list #'(x.sensitivity ... null))
           #:attr reversed #'(x.reversed ...))
  (pattern xs:id/backprop-ids
           #:attr (vars 1) (syntax->list #'(xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'(xs.sensitivity))
           #:attr reversed #'xs.reversed)
  (pattern (x:id/backprop-ids ...+ . xs:id/backprop-ids)
           #:attr (vars 1) (syntax->list #'(x ... xs))
           #:attr (sensitivity-vars 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr (sensitivity-result 1) (syntax->list #'(x.sensitivity ... xs.sensitivity))
           #:attr reversed #'(x.reversed ... . xs.reversed)))

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
     #'(lhs.reversed c)]

    [((lhs) x)
     #'(lhs.reversed x.reversed)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #:with transformed-expr (reverse-transform #'(#%plain-lambda formals M) bound-ids)
     #'(lhs.reversed transformed-expr)]

    [((lhs) (#%plain-app x0 xs ...))
     #'((proc-result lhs.reversed lhs.backprop) (x0.reversed xs.reversed ...))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #'((proc-result lhs.reversed lhs.backprop)
        (if x-test.reversed (x-true.reversed) (x-false.reversed)))]))

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

(define (unknown-transform b bound-ids)
  (define (known-binding? id)
    (or (get-prim-definition id)
        (set-member? (immutable-free-id-set bound-ids) id)))

  (syntax-parse b
    #:local-conventions ([#rx"^x" id]
                         [lhs id])
    #:literal-sets (kernel-literals)
    [((lhs) x)
     (filter-not known-binding? (list #'x))]

    [((lhs) (#%plain-app x0 xs ...))
     (filter-not known-binding? (syntax-e #'(x0 xs ...)))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     (filter-not known-binding? (list #'x-test #'x-true #'x-false))]

    [else '()]))

(define (reverse-transform f [bound-ids '()])
  (syntax-parse f
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [formals lambda-formals/backprop-ids])
    #:literal-sets (kernel-literals)

    [prim:id/backprop-ids #'prim.reversed]

    [{~and lam (#%plain-lambda formals body:nested-let-values)}
     #:with (B ...) #'(body.bindings ...)
     #:with (x ...) #'(B.x ...)
     #:with result:id/backprop-ids #'body.result

     #:with (x-free ...) (free-vars #'lam)

     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(formals.vars ...))
                                      (syntax->list #'(x ...))))]

     #:with (primal-bindings ...)
     (map (curryr ϕ bound-ids*) (syntax-e #'(B ...)))

     #:with (backprop-bindings ...)
     (map (curryr ρ #'box-sensitivities) (reverse (syntax-e #'(B ...))))

     #:with (x-unknown ...)
     (append-map (curryr unknown-transform bound-ids*) (syntax-e #'(B ...)))

     #'(λ formals.reversed
         (destructuring-sum-let*
          ([x-unknown.reversed (unknown-backprop x-unknown 'x-unknown)] ...
            primal-bindings ...)
           (proc-result
            result.reversed
            (λ (result.dummy box-sensitivities)
              (destructuring-sum-let*
                  ([result.sensitivity result.dummy]
                   [x-free.sensitivity (gen-zero)] ...
                   [formals.sensitivity-vars (gen-zero)] ...
                   [x.sensitivity (gen-zero)] ...
                   backprop-bindings ...)
                (list* (list x-free.sensitivity ...)
                       formals.sensitivity-result ...))))))]))
