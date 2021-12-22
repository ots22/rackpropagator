#lang racket/base

(require (for-template racket/base)
         racket/function
         racket/syntax
         racket/set
         syntax/stx
         syntax/parse
         syntax/id-set
         "util.rkt"
         (for-syntax racket/base))

(provide anf-convention
         anf-free-vars
         set!->set-box!

         anf1-val
         anf1-cexpr
         anf1?
         anf1-normalize

         anf2-val
         anf2-binding-expr
         anf2?
         anf2-normalize

         anf1->2

         ;; re-provide anf2-* bindings as anf-*
         (rename-out [anf2-val anf-val]
                     [anf2-binding-expr anf-binding-expr]
                     [anf2-expr anf-expr]
                     [anf2? anf?]
                     [anf2-normalize anf-normalize])

         anf-outer-binding
         anf-expand-expression)

;; ----------------------------------------
;; Conventions and syntax classes

(define-syntax-class lambda-formals
  (pattern (x:id ...)
           #:with (vars ...) #'(x ...)
           #:with temporaries (generate-temporaries #'(x ...)))
  (pattern xs:id
           #:with (vars ...) #'(xs)
           #:with temporaries (generate-temporary #'xs))
  (pattern (x:id ...+ . xs:id)
           #:with (vars ...) #'(x ... xs)
           #:with temporaries #`(#,(generate-temporaries #'(x ...))
                                 . #,(generate-temporary #'xs))))

(define-syntax-class anf-simple-literal
  #:literal-sets (kernel-literals)
  (pattern (quote e))
  (pattern (quote-syntax e {~optional #:local}))
  (pattern (#%variable-reference {~optional id})))

(define-conventions anf1+2-convention
  [#rx"^x" id]
  [formals lambda-formals]

  [c anf-simple-literal]

  ;; e.g. V, but not V*
  [#rx"^V(.*[^*])?$" anf1-val]
  [#rx"^M(.*[^*])?$" anf1-expr]
  [C anf1-cexpr]

  ;; e.g V*, M2*
  [#rx"^V(.*[^*])?[*]$" anf2-val]
  [#rx"^M(.*[^*])?[*]$" anf2-expr]
  [#rx"^B(.*[^*])?[*]$" anf2-binding-expr])

(define-conventions anf-convention
  [#rx"^x" id]
  [formals lambda-formals]

  [c anf-simple-literal]

  [#rx"^V" anf2-val]
  [#rx"^B" anf2-binding-expr]
  [#rx"^M" anf2-expr])

;; ----------------------------------------
;; A-normal form, first kind (ANF1)

;; V
(define-syntax-class anf1-val
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern c)
  (pattern x)
  (pattern (#%plain-lambda formals M)))

;; C
(define-syntax-class anf1-cexpr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern V0)
  (pattern (#%plain-app V0 V ...))
  (pattern (if V0 M-true M-false))
  (pattern (set! x V)))

;; M
(define-syntax-class anf1-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern C)
  (pattern (let-values (((x) C)) M)))

(define anf1? (syntax-class->predicate anf1-expr))

;; ----------------------------------------
;; A-normal form, second kind (ANF2)
;;
;; Note that compared to ANF1:
;;  - The value in 'return' position can be only a named variable
;;  - all function applications are made in a let-values binding (no tail calls)
;;  - function applications involve named variables only

;; V*
(define-syntax-class anf2-val
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern c)
  (pattern x)
  (pattern (#%plain-lambda formals M*)))

;; B*
(define-syntax-class anf2-binding-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern ((x) {~and v V*}))
  (pattern ((x) {~and v (#%plain-app x0 xs ...)}))
  (pattern ((x) {~and v (if x-test
                            (#%plain-app x-true)
                            (#%plain-app x-false))}))
  (pattern ((x) {~and v (set! x1 V*)})))

;; M*
(define-syntax-class anf2-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern x)
  (pattern (let-values (B*) M*)))

(define anf2? (syntax-class->predicate anf2-expr))

;; ----------------------------------------

;; anf-free-vars : anf2? -> free-id-set?
(define (anf-free-vars stx)
  (define Set immutable-free-id-set)
  (syntax-parse stx
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    [x (Set (list #'x))]

    [c (Set '())]

    [(#%plain-lambda formals M*)
     (set-subtract (anf-free-vars #'M*)
                   (Set (syntax-e #'(formals.vars ...))))]

    [(#%plain-app x0 xs ...)
     (Set (syntax-e #'(x0 xs ...)))]

    [(if x-test (#%plain-app x-true) (#%plain-app x-false))
     (Set (list #'x-test #'x-true #'x-false))]

    [(set! x V*)
     (set-add (anf-free-vars #'V*) (list #'x))]

    [(let-values (((x) u)) M*)
     (set-subtract
      (set-union (anf-free-vars #'u) (anf-free-vars #'M*))
      (Set (list #'x)))]))

;; ----------------------------------------

;; Walk stx, which must be a syntax list, calling f on each element: f
;; takes a syntax object and a function (the continuation); k is the
;; initial continuation.
;;
(define (walk-with f stx k)
  (syntax-parse stx
    [() (k #'())]
    [(v vs ...)
     (f #'v (pat-λ (t) (walk-with f #'(vs ...) (pat-λ (ts) (k #'(t . ts))))))]))

;; ----------------------------------------

;; Based on Flanagan, C., Sabry, A., Duba, B. F., & Felleisen, M.,
;; "The essence of compiling with continuations." In proceedings, ACM
;; SIGPLAN 1993 (pp. 237-247).
;;
;; anf-normalize: syntax? (syntax? -> syntax?) -> anf?
(define (anf1-normalize stx [k identity])
  (syntax-parse (syntax-disarm stx #f)
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    [(#%expression e)
     (anf1-normalize #'e k)]

    [(begin us ...+)
     (anf1-normalize #'(let-values () us ...))]

    [(begin0 u0 us ...)
     (anf1-normalize #'(let-values ([(x0) u0])
                         us ...
                         x0))]

    [(#%plain-lambda formals u)
     #:with M (anf1-normalize #'u)
     (k #'(#%plain-lambda formals M))]

    [(#%plain-lambda formals u0 us ...+)
     #:with __ (generate-temporary '__)
     (anf1-normalize #'(#%plain-lambda formals
                         (let-values (((__) u0))
                           us ...))
                     k)]

    [(let-values (((x0) u1)) u2)
     #:with M2 (anf1-normalize #'u2 k)
     (anf1-normalize #'u1 (pat-λ (r) #'(let-values (((x0) r)) M2)))]

    [(let-values (((x0) u1) ((xs) us) ...) u2)
     (anf1-normalize #'(let-values (((x0) u1))
                        (let-values (((xs) us) ...)
                          u2))
                     k)]

    [(let-values (binding-forms ...) body0 body ... bodyn)
     #:with __ (generate-temporary '__)
     (anf1-normalize #'(let-values (binding-forms ...)
                         (let-values ([(__) body0])
                           body ... bodyn))
                     k)]

    [(letrec-values (binding-forms ...) body0 body ... bodyn)
     #:with __ (generate-temporary '__)
     (anf1-normalize #'(letrec-values (binding-forms ...)
                         (let-values ([(__) body0])
                           body ... bodyn))
                     k)]

    [(let-values () u1)
     (anf1-normalize #'u1 k)]

    [(letrec-values (((xs) us) ...) u)
     (anf1-normalize #'(let-values (((xs) '()) ...)
                         (set! xs us) ...
                         u)
                     k)]

    [(if u1 u2 u3)
     #:with M2 (anf1-normalize #'u2)
     #:with M3 (anf1-normalize #'u3)
     (anf1-normalize-name #'u1 (pat-λ (t) (k #'(if t M2 M3))))]

    [(#%plain-app u us ...)
     (walk-with anf1-normalize-name
                #'(u us ...)
                (pat-λ (r) (k #'(#%plain-app . r))))]

    [(set! x u)
     #:with set!-void (generate-temporary 'set!-void)
     #:with body (k #'set!-void)
     (anf1-normalize-name
      #'u
      (pat-λ (t) #'(let-values (((set!-void) (set! x t)))
                     body)))]

    [V (k #'V)]))

(define (anf1-normalize-name stx k)
  (anf1-normalize
   stx
   (λ (u) (syntax-parse u
            #:conventions (anf1+2-convention)
            #:literal-sets (kernel-literals)
            [V (k u)]
            [_ #:with x (generate-temporary)
               #:with u u
               #:with body (k #'x)
               #'(let-values (((x) u))
                   body)]))))

;; ----------------------------------------

;; anf1->2: anf1? -> anf2?
(define (anf1->2 stx)
  (syntax-parse stx
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    [(let-values (((x) V)) M)
     #:with M* (anf1->2 #'M)
     #:with V* (anf2-normalize-value #'V)
     #'(let-values (((x) V*)) M*)]

    ;; Handle the base case for each part of the conditional separately
    [(if x-test (#%plain-app x-true) (#%plain-app x-false))
     #:with x (generate-temporary #'x)
     #'(let-values (((x) (if x-test
                             (#%plain-app x-true)
                             (#%plain-app x-false))))
         x)]

    [(if V M1 M2)
     #:with M1* (anf1->2 #'M1)
     #:with M2* (anf1->2 #'M2)
     #:with (x-test x-true x-false x)
            (generate-temporaries #'(x-test x-true x-false x))
     #'(let-values (((x-test) V))
         (let-values (((x-true) (#%plain-lambda () M1*)))
           (let-values (((x-false) (#%plain-lambda () M2*)))
             (let-values (((x) (if x-test
                                   (#%plain-app x-true)
                                   (#%plain-app x-false))))
               x))))]

    [(let-values (((x) (#%plain-app V Vs ...))) M)
     #:with M* (anf1->2 #'M)
     (walk-with anf2-lift-value
                #'(V Vs ...)
                (pat-λ (r) #'(let-values (((x) (#%plain-app . r)))
                               M*)))]

    ;; Handle the base case for each part of the conditional separately
    [(let-values (((x) (if x-test
                           (#%plain-app x-true)
                           (#%plain-app x-false))))
       M)
     #:with M* (anf1->2 #'M)
     #'(let-values (((x) (if x-test
                             (#%plain-app x-true)
                             (#%plain-app x-false))))
         M*)]

    [(let-values (((x) (if V M1 M2))) M)
     #:with M* (anf1->2 #'M)
     #:with M1* (anf1->2 #'M1)
     #:with M2* (anf1->2 #'M2)
     #:with (x-test x-true x-false)
            (generate-temporaries #'(x-test x-true x-false))
     #'(let-values (((x-test) V))
         (let-values (((x-true) (#%plain-lambda () M1*)))
           (let-values (((x-false) (#%plain-lambda () M2*)))
             (let-values (((x) (if x-test
                                   (#%plain-app x-true)
                                   (#%plain-app x-false))))
               M*))))]

    [(let-values (((x-void) (set! x V))) M)
     #:with M* (anf1->2 #'M)
     #:with V* (anf2-normalize-value #'V)
     #'(let-values (((x-void) (set! x V*))) M*)]

    [(#%plain-app V Vs ...)
     #:with x (generate-temporary)
     (anf1->2
      #'(let-values (((x) (#%plain-app V Vs ...)))
          x))]

    [(set! x V)
     #:with x-void (generate-temporary 'set!-void)
     (anf1->2
      #'(let-values (((set!-void) (set! x V)))
          set!-void))]

    [x #'x]

    [V #:with x (generate-temporary)
       #:with V* (anf2-normalize-value #'V)
       #'(let-values (((x) V*)) x)]))

(define (anf2-normalize-value v)
  (syntax-parse v
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    [(#%plain-lambda formals M)
     #:with M* (anf1->2 #'M)
     #'(#%plain-lambda formals M*)]

    [V* #'V*]))

(define (anf2-lift-value v k)
  (syntax-parse v
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    [x (k v)]
    [(#%plain-lambda formals M)
     #:with M* (anf1->2 #'M)
     #:with t (generate-temporary)
     #:with body (k #'t)
     #'(let-values (((t) (#%plain-lambda formals M*)))
         body)]
    [c
     #:with t (generate-temporary)
     #:with body (k #'t)
     #'(let-values (((t) c))
         body)]))

(define (anf2-normalize stx)
  (anf1->2 (anf1-normalize stx)))

;; ----------------------------------------

;; set!-target-ids : anf2 -> free-id-set?
;;
;; Return the set of identifiers in stx that are mutated with set!
(define (set!-target-ids stx)
  (syntax-parse stx
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)

    [(let-values (((x) (#%plain-lambda formals M1))) M2)
     (set-union (set!-target-ids #'M1)
                (set!-target-ids #'M2))]

    [(let-values (((x) (set! x1 V))) M)
     (set-add (set!-target-ids #'M) #'x1)]

    ;; remaining let-values cases
    [(let-values (B) M)
     (set!-target-ids #'M)]

    [x (immutable-free-id-set)]))

;; set!->set-box! : anf2? -> anf2?
;;
;; Convert all bindings that are mutated with set! to boxes and
;; set-box!
(define (set!->set-box! stx)
  (define targets (set!-target-ids stx))

  (define (target-unbox id)
    (if (set-member? targets id)
        #`(#%plain-app unbox #,id)
        id))

  (define (target-box-value id v)
    (if (set-member? targets id)
        #`(#%plain-app box #,v)
        v))

  (define rec
    (syntax-parser
      #:conventions (anf-convention)
      #:literal-sets (kernel-literals)

      [x (target-unbox #'x)]

      [(let-values (((x) c)) M)
       #:with $M (rec #'M)
       #:with $c (target-box-value #'x #'c)
       #'(let-values (((x) $c)) $M)]

      [(let-values (((x) x1)) M)
       #:with $M (rec #'M)
       #:with $x1 (target-box-value #'x (target-unbox #'x1))
       #'(let-values (((x) $x1)) $M)]

      ;; straightforward case: none of formals is a target of set!
      [(let-values (((x) (#%plain-lambda formals M1))) M2)
       #:when (andmap (λ (arg) (not (set-member? targets arg)))
                      (syntax-e #'(formals.vars ...)))
       #:with $M1 (rec #'M1)
       #:with $M2 (rec #'M2)
       #:with lam (target-box-value #'x #'(#%plain-lambda formals $M1))
       #'(let-values (((x) lam)) $M2)]

      ;; when one or more lambda formals is a set! target, introduce
      ;; extra let bindings, boxed as necessary
      [(let-values (((x) (#%plain-lambda formals M1))) M2)
       #:when (ormap (λ (arg) (set-member? targets arg))
                     (syntax-e #'(formals.vars ...)))
       #:with $M1 (rec #'M1)
       #:with $M2 (rec #'M2)
       #:with formals*:lambda-formals #'formals.temporaries
       #:with (formals*-boxed-vars ...) (map target-box-value
                                             (syntax-e #'(formals.vars ...))
                                             (syntax-e #'(formals*.vars ...)))
       #:with lam #'(#%plain-lambda formals*
                      (let-values ([(formals.vars) formals*-boxed-vars] ...)
                        $M1))

       #:with lam* (target-box-value #'x #'lam)
       #'(let-values (((x) lam)) $M2)]

      [(let-values (((x) (#%plain-app x0 xs ...))) M)
       #:with $M (rec #'M)
       #:with ($x0 $xs ...) (stx-map target-unbox #'(x0 xs ...))
       #:with app (target-box-value #'x #'(#%plain-app $x0 $xs ...))
       #'(let-values (((x) app)) $M)]

      [(let-values (((x) (if x-test
                             (#%plain-app x-true)
                             (#%plain-app x-false))))
         M)
       #:with $M (rec #'M)
       #:with ($x-test $x-true $x-false)
       (stx-map target-unbox #'(x-test x-true x-false))
       #:with branch (target-box-value #'x #'(if $x-test
                                                 (#%plain-app $x-true)
                                                 (#%plain-app $x-false)))
       #'(let-values (((x) branch)) $M)]

      [(let-values (((x) (set! x1 c))) M)
       #:with $M (rec #'M)
       #:with st (target-box-value #'x #'(#%plain-app set-box! x1 c))
       #'(let-values (((x) st)) $M)]

      [(let-values (((x) (set! x1 x2))) M)
       #:with $M (rec #'M)
       #:with $x2 (target-unbox #'x2)
       #:with st (target-box-value #'x #'(#%plain-app set-box! x1 $x2))
       #'(let-values (((x) st)) $M)]

      ;; Two lambda cases as above, but when used in set!
      [(let-values (((x) (set! x1 (#%plain-lambda formals M1)))) M2)
       #:when (andmap (λ (arg) (not (set-member? targets arg)))
                      (syntax-e #'(formals.vars ...)))
       #:with $M1 (rec #'M1)
       #:with $M2 (rec #'M2)
       #:with st (target-box-value
                  #'x
                  #'(#%plain-app set-box! x1 (#%plain-lambda formals $M1)))
       #'(let-values (((x) st)) $M2)]

      [(let-values (((x) (set! x1 (#%plain-lambda formals M1)))) M2)
       #:when (ormap (λ (arg) (set-member? targets arg))
                     (syntax-e #'(formals.vars ...)))
       #:with $M1 (rec #'M1)
       #:with $M2 (rec #'M2)
       #:with formals*:lambda-formals #'formals.temporaries
       #:with (formals*-boxed-vars ...) (map target-box-value
                                             (syntax-e #'(formals.vars ...))
                                             (syntax-e #'(formals*.vars ...)))
       #:with lam #'(#%plain-lambda formals*
                      (let-values ([(formals.vars) formals*-boxed-vars] ...)
                        $M1))
       #:with st (target-box-value
                  #'x
                  #'(#%plain-app set-box! x1 lam))
       #'(let-values (((x) st)) $M2)]))

  (anf2-normalize (rec stx)))

;; ----------------------------------------

;; anf-expand-expression : expr? -> anf2?
(define (anf-expand-expression expr)
  (set!->set-box! (anf2-normalize (local-expand expr 'expression '()))))

;; Given an anf2 expression, extract the outermost let-bound expression
(define (anf-outer-binding expr)
  (syntax-parse expr
    #:literal-sets (kernel-literals)
    [(let-values (((x-result-arg) e*)) x-result-final)

     #:fail-unless (and (identifier? #'x-result-final)
                        (free-identifier=? #'x-result-arg #'x-result-final))
                   "Result of anf-normalize had an unexpected form"
     #'e*]

    [other #f]))
