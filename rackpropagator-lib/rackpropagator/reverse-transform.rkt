#lang racket/base

(require (for-template (except-in racket/base apply)
                       "apply.rkt"
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
;; Renaming

(define (id-modifier [pre ""] [post ""])
  (let ([ids (make-free-id-table)])
    (λ (id)
      (let ([name (format-id #f "~a~a~a" pre id post)])
        (dict-ref! ids id (generate-temporary name))))))

(define backpropagator (id-modifier "<-" "-"))
(define sensitivity (id-modifier "^" "-"))
(define dummy (id-modifier "^" "-dummy-"))
(define tagged (id-modifier "" "*"))


;; ----------------------------------------
;; State: identifiers of introduced reverse-transformed primitives

(define prim->reverse-intro (make-free-id-table))

(define (get-tagged id)
  (dict-ref prim->reverse-intro id (tagged id)))

;; reverse-tag : (listof identifier?) identifier? -> identifier?
(define (reverse-tag id)
  (dict-ref prim->reverse-intro id
            (λ ()
              (cond
                [(get-prim-definition id) => (curry introduce-prim-def! id)]
                [else (tagged id)]))))

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


;; ----------------------------------------
;; Syntax classes

(define-syntax-class id/backprop-ids
  (pattern x:id
           #:attr sensitivity (sensitivity #'x)
           #:attr backprop (backpropagator #'x)
           #:attr dummy (dummy #'x)
           #:attr tagged (reverse-tag #'x)))

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
  (pattern (let-values (B) body:nested-let-values)
           #:attr (bindings 1) (cons #'B (syntax-e #'(body.bindings ...)))
           #:attr result #'body.result)
  (pattern body:id
           #:attr (bindings 1) null
           #:attr result #'body))

;; ----------------------------------------


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
     #'(lhs.tagged c)]

    [((lhs) x)
     #:with x-tagged-or-prim (reverse-tag #'x)
     #'(lhs.tagged x-tagged-or-prim)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #:with transformed-expr (reverse-transform #'(#%plain-lambda formals M)
                                                bound-ids)
     #'(lhs.tagged transformed-expr)]

    [((lhs) (#%plain-app x0 xs ...))
     #:with (x0-tagged xs-tagged ...) (stx-map reverse-tag #'(x0 xs ...))
     #'((proc-result lhs.tagged lhs.backprop) (x0-tagged xs-tagged ...))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #:with x-test-tagged (reverse-tag #'x-test)
     #:with x-true-tagged (reverse-tag #'x-true)
     #:with x-false-tagged (reverse-tag #'x-false)
     #'((proc-result lhs.tagged lhs.backprop)
        (if x-test-tagged (x-true-tagged) (x-false-tagged)))]
    ;
    ))

(define (ρ b box-adjoints)
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
        (lhs.backprop lhs.sensitivity box-adjoints))]

    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #:with x-test-tagged (reverse-tag #'x-test)
     #'((x-true.sensitivity x-false.sensitivity)
        (let ([b (car (lhs.backprop lhs.sensitivity box-adjoints))])
          (if x-test-tagged
              (list b (gen-zero))
              (list (gen-zero) b))))]
    ;;
    ))

(define (get-unknown-backprops b bound-ids)

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

    [else '()]
    ;;
    ))

(define (reverse-transform f [bound-ids '()])
  (syntax-parse f
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [formals lambda-formals/backprop-ids])
    #:literal-sets (kernel-literals)

    ;;
    [prim:id (reverse-tag #'prim)]

    ;;
    [{~and lam
           (#%plain-lambda formals
             body:nested-let-values)}

     #:with (B ...) #'(body.bindings ...)
     #:with (x ...) #'(B.x ...)
     #:with result:id/backprop-ids #'body.result

     #:with (x-free ...) (free-vars #'lam)

     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(formals.vars ...))
                                      (syntax->list #'(x ...))))]

     #:with result-tagged (reverse-tag #'result)

     #:with (primal-bindings ...)
     (map (curryr ϕ bound-ids*) (syntax-e #'(B ...)))

     #:with (backprop-bindings ...)
     (map (curryr ρ #'box-adjoints) (reverse (syntax-e #'(B ...))))

     #:with (x-unknown ...)
     (append-map (curryr get-unknown-backprops bound-ids*) (syntax-e #'(B ...)))

     #'(λ formals.tagged
         (destructuring-sum-let*
          ([x-unknown.tagged (unknown-backprop x-unknown 'x-unknown)] ...
            primal-bindings ...)
           (proc-result
            result-tagged
            (λ (result.dummy box-adjoints)
              (destructuring-sum-let*
                  ([x-free.sensitivity (gen-zero)] ...
                   [formals.sensitivity-vars (gen-zero)] ...
                   [x.sensitivity (gen-zero)] ...
                   [result.sensitivity result.dummy]
                   backprop-bindings ...)
                (list* (list x-free.sensitivity ...)
                       formals.sensitivity-result ...))))))]
    ;;
    ))
