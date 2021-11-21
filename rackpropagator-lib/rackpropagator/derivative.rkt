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
;; (define-for-syntax backprop-intro->prim (make-free-id-table))


(define-for-syntax (get-anf-outer-def expr)
  (syntax-parse expr
    #:literal-sets (kernel-literals)
    [(let-values (((x-result-arg) e*)) x-result-final)
     #:fail-unless (free-identifier=? #'x-result-arg #'x-result-final)
     "Result of anf-normalize had an unexpected form"
     
     #'e*]))

(define-for-syntax (introduce-primitive p)
  (if (dict-has-key? prim->backprop-intro p)
      (dict-ref prim->backprop-intro p)
      (let ([p*-def (get-prim-definition p)])
        (if p*-def
            (let ([p*-lifted (syntax-local-lift-expression p*-def)])
              (dict-set! prim->backprop-intro p p*-lifted)
              (let* ([p*-def-anf (set!->set-box! (anf-normalize (local-expand p*-def 'expression '())))]
                     [p*-def-anf-outer (get-anf-outer-def p*-def-anf)]
                     
                     ;[__ (displayln (syntax->datum p*-def-anf-outer))]
                     
                     [D*p* (reverse-transform p*-def-anf-outer)])
                (set!-prim-definition p*-lifted D*p*)
                p*-lifted))
            (tagged p)))))
  
;; (dict-ref! prim->backprop-intro p
;;              (λ ()
;;                (let* ([p*-def (get-prim-definition p)]
;;                       [p*-lifted (syntax-local-lift-expression p*-def)]
                      
;;                       [p*-def-anf (set!->set-box! (anf-normalize (local-expand p*-def 'expression '())))]
;;                       [p*-def-anf-outer (get-anf-outer-def p*-def-anf)]

;;                       [__ (displayln (syntax->datum p*-def-anf-outer))]

;;                       [D*p* (reverse-transform p*-def-anf-outer)])
;;                  (set!-prim-definition p*-lifted D*p*)
;;                  p*-lifted))))

(define-for-syntax (id-modifier [pre ""] [post ""])
  (let ([ids (make-free-id-table)])
    (λ (id)
      (let ([name (format-id #f "~a~a~a" pre id post)])
        (dict-ref! ids id (generate-temporary name))))))

(define-for-syntax (free-vars stx)
  (set->list (anf-free-vars stx)))

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
     #:with x-tagged-or-prim (introduce-primitive #'x)
     #'(lhs.tagged x-tagged-or-prim)]

    [((lhs) (#%plain-lambda formals M))
     #:with (x-free ...) (free-vars #'(#%plain-lambda formals M))
     #:with transformed-expr (reverse-transform #'(#%plain-lambda formals M) bound-ids)
     #'(lhs.tagged transformed-expr)]

    [((lhs) (#%plain-app x0 xs ...))
     #:with (x0-tagged xs-tagged ...) (stx-map introduce-primitive #'(x0 xs ...))
     #'((proc-result lhs.tagged lhs.backprop) (x0-tagged xs-tagged ...))]

    ;; what if x-test (and maybe x-true/false?) are primitives?
    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     ;#:do [(stx-map introduce-primitive #'(x-test x-true x-false))]
     #'((proc-result lhs.tagged lhs.backprop)
        (if x-test.tagged (x-true.tagged) (x-false.tagged)))]
    ;
    ))

(define-for-syntax (ρ b)
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

    ;; what if x-test (and maybe x-true/false?) are primitives?
    [((lhs) (if x-test (#%plain-app x-true) (#%plain-app x-false)))
     #'((x-true.sensitivity x-false.sensitivity)
        (let ([b (car (lhs.backprop lhs.sensitivity))])
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

     #:with result-tagged (introduce-primitive #'result)

     #:with (x-free ...) (free-vars #'lam)

     #:do [(define bound-ids* (append bound-ids
                                      (syntax->list #'(formals.vars ...))
                                      (syntax->list #'(x ...))))]

     #:with (primal-bindings ...)
     (map (curryr ϕ bound-ids*) (syntax-e #'(B ...)))

     #:with (backprop-bindings ...)
     (map ρ (reverse (syntax-e #'(B ...))))

     #'(λ formals.tagged
         (destructuring-sum-let* (primal-bindings ...)
           (proc-result
            result-tagged
            (λ (result.dummy)
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
     #:with e-anf
     (set!->set-box! (anf-normalize (local-expand #'e 'expression '())))

     #:with (let-values (((x-result-arg) e*)) x-result-final) #'e-anf
     #:fail-unless (free-identifier=? #'x-result-arg #'x-result-final)
     "Result of anf-normalize had an unexpected form"

     #:with De* (reverse-transform #'e*)

     #'(let ([box-adjoints (make-hasheq)])
         (syntax-parameterize ([current-box-adjoints
                                (make-rename-transformer #'box-adjoints)])
           (let ([D+f De*])
             (λ xs
               (let ([primal+backprop (apply D+f xs)])
                 (proc-result
                  (primal primal+backprop)
                  (λ Aw
                    (coerce-zero
                     ;; drop terms from closed-over variables
                     (cdr (apply (backprop primal+backprop) Aw))
                     xs))))))))]))

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
     #:with e-anf
     (set!->set-box! (anf-normalize (local-expand #'e 'expression '())))

     #:with (let-values (((x-result-arg) e*)) x-result-final) #'e-anf
     #:fail-unless (free-identifier=? #'x-result-arg #'x-result-final)
     "Result of anf-normalize had an unexpected form"

     #:with De* (reverse-transform #'e*)

     #'De*]))


(define-syntax (define/backprop stx)
  (syntax-parse stx
    ;; TODO formals
    [(_ (f xs ...) body ...)
     #:with (xs* ...) (generate-temporaries #'(xs ...))
     #:with D*f (local-expand #'(D* (λ (xs* ...) (define (f xs ...) body ...) (f xs* ...)))
                              'expression
                              #f)
     #'(begin
         (define (f xs ...) body ...)
         (local-register-primitive! f D*f))]

    ;; [(_ (f . xs) body ...)
    ;;  #:with xs* (generate-temporaries #'xs)
    ;;  #:with D*f (local-expand #'(D* (λ xs* (define (f . xs) body ...) (apply f xs*)))
    ;;                           'expression
    ;;                           #f)
    ;;  #'(begin
    ;;      (define (f . xs) body ...)
    ;;      (local-register-primitive! f D*f))]
    
    ;; [(_ f body ...)
    ;;  #:with D*f (local-expand #'(D* (λ xs (define f body ...) (apply f xs)))
    ;;                           'expression
    ;;                           #f)
    ;;  #'(begin
    ;;      (define f body ...)
    ;;      (local-register-primitive! f D*f))]
    ))

;; this isn't going to work I think - recursive call won't terminate
;; (define-syntax (D+-nonlocal stx)
;;   (syntax-parse stx
;;     #:literal-sets (kernel-literals)
;;     [(_ e)
;;      #:with e-anf
;;      (set!->set-box! (anf-normalize (local-expand #'e 'expression '())))

;;      #:with (let-values (((x-result-arg) e*)) x-result-final) #'e-anf
;;      #:fail-unless (free-identifier=? #'x-result-arg #'x-result-final)
;;      "Result of anf-normalize had an unexpected form"

;;      #:with (De* (prim:id prim-intro:id) ...) (reverse-transform #'e*)

;;      #:with ((distinct-prim distinct-prim-intro) ...)
;;      (remove-duplicates (syntax-e #'((prim prim-intro) ...))
;;                         free-identifier=? #:key stx-car)

;;      #:with (prim-def ...) #'((prim-definition distinct-prim) ...)

;;      #:with (distinct-prim-intro-ids ...) (stx-map syntax-local-lift-expression #'(prim-def ...))

;;      ;; wrong number of ellipsis
;;      #:with D*f (local-expand #'(D+-nonlocal (λ xs (define f prim-def) (apply f xs)))
;;                               'expression
;;                               #f)
;;      (define ignored (stx-map syntax-local-lift-expression 
;;                               #'(local-register-primitive! distinct-prim-intro-ids D*f)))
                              
;;      #'(let ([box-adjoints (make-hasheq)])
;;          (syntax-parameterize ([current-box-adjoints
;;                                 (make-rename-transformer #'box-adjoints)]

;;                                [current-non-prim-transform
;;                                 (syntax-parser
;;                                   [(_ other)
;;                                    #'(let ([other* (strip-backprop other)])
;;                                        (if (procedure? other*)
;;                                            (λ xs
;;                                              (proc-result
;;                                               (apply other* xs)
;;                                               (λ (Aw)
;;                                                 (if (gen-zero? Aw)
;;                                                     Aw
;;                                                     (unknown-backprop 'other)))))
;;                                            other))])])

;;              (let ([D+f De*])
;;                (λ xs
;;                  (let ([primal+backprop (apply D+f xs)])
;;                    (proc-result
;;                     (primal primal+backprop)
;;                     (λ Aw
;;                       (coerce-zero
;;                        ;; drop terms from closed-over variables
;;                        (cdr (apply (backprop primal+backprop) Aw))
;;                        xs))))))))]))

;; (define Df (D+-nonlocal (λ (x y) (+ x y))))

