#lang racket/base

(require racket/stxparam
         "apply.rkt"
         "builtins.rkt"
         "prim-definition.rkt"
         "primitives.rkt"
         "sum-let.rkt"
         (for-syntax (except-in racket/base apply)
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
                     "anf.rkt"
                     "apply.rkt"
                     "builtins.rkt"))

(provide D+)

(define-for-syntax prim->backprop-intro (make-free-id-table))


(define-for-syntax (anf-outer-binding expr)
  (syntax-parse expr
    #:literal-sets (kernel-literals)
    [(let-values (((x-result-arg) e*)) x-result-final)

     #:fail-unless (free-identifier=? #'x-result-arg #'x-result-final)
     "Result of anf-normalize had an unexpected form"

     #'e*]))

(define-for-syntax (free-vars stx)
  (set->list (anf-free-vars stx)))

(define-for-syntax (anf-expand-expression expr)
  (set!->set-box! (anf-normalize (local-expand expr 'expression '()))))


(define-for-syntax (id-modifier [pre ""] [post ""])
  (let ([ids (make-free-id-table)])
    (λ (id)
      (let ([name (format-id #f "~a~a~a" pre id post)])
        (dict-ref! ids id (generate-temporary name))))))

(begin-for-syntax
  (define backpropagator (id-modifier "<-" "-"))
  (define sensitivity (id-modifier "^" "-"))
  (define dummy (id-modifier "^" "-dummy-"))
  (define tagged (id-modifier "" "*"))

  (define-syntax-class id/backprop-ids
    (pattern x:id
             #:attr sensitivity (sensitivity #'x)
             #:attr backprop (backpropagator #'x)
             #:attr dummy (dummy #'x)
             #:attr tagged (dict-ref prim->backprop-intro #'x (tagged #'x))))

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
             #:attr result #'body)))


;; introduce-prim-def! identifier? syntax? -> identifier?
;;
;; Given p, an identifier representing a primitive, and p*-def,
;; syntax representing the definition of its reverse transformation,
;; lifts p*-def and returns the lifted identifier.  In addition,
;; p*-def is itself reverse transformed, and registered as the
;; primitive definition of the lifted identifier.
(define-for-syntax (introduce-prim-def! p p*-def)
  (let ([p*-lifted (syntax-local-lift-expression p*-def)])
    (dict-set! prim->backprop-intro p p*-lifted)
    (set-prim-definition! p*-lifted
                          (reverse-transform
                           (anf-outer-binding
                            ;; box-adjoints needs to have a parameterization here
                            (anf-expand-expression p*-def))))
    p*-lifted))

;; reverse-tag : (listof identifier?) identifier? -> identifier?
(define-for-syntax (reverse-tag bound-ids id)
  (dict-ref prim->backprop-intro id
            (λ ()
              (cond
                [(set-member? (immutable-free-id-set bound-ids) id) (tagged id)]
                ;; handle case of current-non-prim-transform here? Or
                ;; in introduce-prim-def!? (expands to #'(if ...)) so
                ;; reverse-transform won't work with it. Must produce
                ;; an identifier.
                [(get-prim-definition id) => (curry introduce-prim-def! id)]
                [else id]))))


;; TODO unknown-backprops
;;  - reverse-tag looks up prim id, otherwise always returns 'tagged'
;;  - have a separate pass (to phi, rho) that checks for unknown identifiers
;;  - introduce these as let bindings (to the 'unknown' handling
;;    expression) in the function built by reverse-transform
;;    - if an unknown is used by the input function *and* a nested
;;      function, this will introduce the same identifier in each of the
;;      transformed functions, but, I can't see why that wouldn't work.

;;  - potential drawback - is there a more elegant way of doing this?
;;    Differentiating repeatedly would cause a lot of these definitions
;;    to be introduced.
;;    - can't include as lifts, since they might refer to 'more local'
;;      bindings than at module level (consider D+ wrapped in a let)
;;    - maybe just minimise the code: have a builtin 'unknown' again
;;      (that takes the value and a symbol for its name in the error
;;      reporting)
;;    - add a syntax property to the introduced definition (but then
;;      what? ignore them?)

;; TODO box-adjoints
;;  - add extra argument and pass to each backpropagator
;;  - some care needed: need to use the same one in each nested
;;    reverse-transform (part of the same derivative), but a different
;;    one for each higher derivative.
;;  - reverse-mode produces a function with this signature too:
;;    perhaps okay to rely on the caller to get this right, and
;;    otherwise this is self-working

;; TODO filter out extra closure sensitivities
;;  - should be straightforward now we can check for both bound-vars
;;    and prims

;; Note: takes a 'let-values' style binding, and produces a binding
;; form for sum-destructuring-let (sheds one level of parens around
;; the id)
(define-for-syntax (ϕ b bound-ids)
  (syntax-parse b
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [lhs id/backprop-ids])
    #:literal-sets (kernel-literals)
    [((lhs) c)
     #'(lhs.tagged c)]

    [((lhs) x)
     #:with x-tagged-or-prim (reverse-tag bound-ids #'x)
     #'(lhs.tagged x-tagged-or-prim)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #:with transformed-expr (reverse-transform #'(#%plain-lambda formals M) bound-ids)
     #'(lhs.tagged transformed-expr)]

    [((lhs) (#%plain-app x0 xs ...))
     #:with (x0-tagged xs-tagged ...) (stx-map (curry reverse-tag bound-ids) #'(x0 xs ...))
     #'((proc-result lhs.tagged lhs.backprop) (x0-tagged xs-tagged ...))]

    ;; what if x-test (and maybe x-true/false?) are primitives?
    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     ;#:do [(stx-map introduce-primitive #'(x-test x-true x-false))]
     #'((proc-result lhs.tagged lhs.backprop)
        (if x-test.tagged (x-true.tagged) (x-false.tagged)))]
    ;
    ))

(define-for-syntax (ρ b box-adjoints)
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

    ;; what if x-test (and maybe x-true/false?) are primitives?
    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #'((x-true.sensitivity x-false.sensitivity)
        (let ([b (car (lhs.backprop lhs.sensitivity box-adjoints))])
          (if x-test.tagged
              (list b (gen-zero))
              (list (gen-zero) b))))]
    ;;
    ))

(define-for-syntax (reverse-transform f [bound-ids '()])
  (syntax-parse f
    #:conventions (anf-convention)
    #:local-conventions ([#rx"^x" id/backprop-ids]
                         [formals lambda-formals/backprop-ids])
    #:literal-sets (kernel-literals)

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

     #:with result-tagged (reverse-tag bound-ids* #'result)

     #:with (primal-bindings ...)
     (map (curryr ϕ bound-ids*) (syntax-e #'(B ...)))

     #:with (backprop-bindings ...)
     (map (curryr ρ #'box-adjoints) (reverse (syntax-e #'(B ...))))

     #'(λ formals.tagged
         (destructuring-sum-let* (primal-bindings ...)
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


(define-syntax (D+ stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ e)
     #:with e-anf (anf-expand-expression #'e)
     #:with e* (anf-outer-binding #'e-anf)
     #:with De* (reverse-transform #'e*)
     #'(let ([Abox (make-hasheq)])
         (let ([D+f De*])
           (λ xs
             (let ([primal+backprop (apply D+f xs)])
               (proc-result
                (primal primal+backprop)
                (λ (Aw)
                  (coerce-zero
                   ;; drop terms from closed-over variables
                   (cdr ((backprop primal+backprop) Aw Abox))
                   xs)))))))]))

;; TODO better name!
;;
;; This is like D+, but does not:
;;  - syntax-parameterize with current-non-prim, box-adjoints
;;  - drop closure terms
;;  - apply coerce-zero to the result
;;
;; It is more 'composable' and is intended to write some
;; differentiation primitives
(define-syntax (D* stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ e)
     #:with e-anf (anf-expand-expression #'e)
     #:with e* (anf-outer-binding #'e-anf)
     #:with De* (reverse-transform #'e*)
     #'De*]))








;; (define-syntax (define/backprop stx)
;;   (syntax-parse stx
;;     ;; TODO formals
;;     [(_ (f xs ...) body ...)
;;      #:with (xs* ...) (generate-temporaries #'(xs ...))
;;      #:with D*f (local-expand #'(D* (λ (xs* ...) (define (f xs ...) body ...) (f xs* ...)))
;;                               'expression
;;                               #f)
;;      #'(begin
;;          (define (f xs ...) body ...)
;;          (local-register-primitive! f D*f))]

;;     ;; [(_ (f . xs) body ...)
;;     ;;  #:with xs* (generate-temporaries #'xs)
;;     ;;  #:with D*f (local-expand #'(D* (λ xs* (define (f . xs) body ...) (apply f xs*)))
;;     ;;                           'expression
;;     ;;                           #f)
;;     ;;  #'(begin
;;     ;;      (define (f . xs) body ...)
;;     ;;      (local-register-primitive! f D*f))]

;;     ;; [(_ f body ...)
;;     ;;  #:with D*f (local-expand #'(D* (λ xs (define f body ...) (apply f xs)))
;;     ;;                           'expression
;;     ;;                           #f)
;;     ;;  #'(begin
;;     ;;      (define f body ...)
;;     ;;      (local-register-primitive! f D*f))]
;;     ))
