#lang racket/base

(require (for-template racket/base)
         (for-syntax racket/base)
         racket/function
         racket/syntax
         syntax/stx
         syntax/parse
         "util.rkt")

(provide (rename-out [anf3-convention anf-convention]
                     [anf3-val anf-val]
                     [anf3-binding-expr anf-binding-expr]
                     [anf3-expr anf-expr]
                     [anf3? anf?]
                     [anf3-normalize anf-normalize]))

(module+ test
  (require rackunit))


;; ----------------------------------------

;; combining-letrec is a placeholder identifier for the output of
;; anf1/anf2, and should not appear in the final transformed output,
;; of anf3
(define-syntax (combining-letrec stx)
  (raise-syntax-error #f "invalid syntax" stx))

;; ----------------------------------------
;; Conventions and syntax classes

(define-syntax-class lambda-formals
  (pattern (x:id ...)
           #:attr vars #'(x ...))
  (pattern xs:id
           #:attr vars #'(xs))
  (pattern (x:id ...+ . xs:id)
           #:attr vars #'(x ... xs)))

(define-syntax-class let-id
  #:literal-sets (kernel-literals)
  (pattern {~or* let-values letrec-values combining-letrec}))

(define-syntax-class anf-const
  (pattern a:boolean)
  (pattern a:number))

(define-conventions anf1+2-convention
  [#rx"^x" id]
  [formals lambda-formals]
  [let-or-letrec let-id]

  [c anf-const]

  [#rx"^V" anf1-val]
  [#rx"^M" anf1-expr]

  [#rx"^W" anf2-val]
  [#rx"^S" anf2-expr])

(define-conventions anf3-convention
  [#rx"^x" id]
  [formals lambda-formals]
  [let-or-letrec let-id]

  [c anf-const]

  [#rx"^V" anf3-val]
  [#rx"^B" anf3-binding-expr]
  [#rx"^M" anf3-expr])

;; ----------------------------------------
;; A-normal form, first kind (ANF1)

(define-syntax-class anf1-val
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda formals M)))

(define-syntax-class anf1-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  #:literals (combining-letrec)
  (pattern V0) ; return
  (pattern (#%plain-app V0 V ...)) ; tail call
  (pattern (combining-letrec (((x) V0)) M)) ; bind
  (pattern (combining-letrec (((x) (#%plain-app V0 V ...))) M)) ; call
  (pattern (if V0 M-true M-false))) ; branch

(define anf1? (syntax-class->predicate anf1-expr))

;; ----------------------------------------
;; A-normal form, second kind (ANF2)
;;
;; Note that compared to ANF1:
;;  - The value in 'return' position can be only a named variable
;;  - all function applications are mode in a let-values binding (no tail calls)
;;  - function applications involve named variables only

(define-syntax-class anf2-val
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda formals S)))

(define-syntax-class anf2-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern x)
  (pattern (combining-letrec (((x) W)) S))
  (pattern (combining-letrec (((x) (#%plain-app x0 xs ...))) S))
  (pattern (combining-letrec (((x) (if x-test
                                       (#%plain-app x-true)
                                       (#%plain-app x-false)))) S)))

(define anf2? (syntax-class->predicate anf2-expr))

;; ----------------------------------------
;; A-normal form, third kind (ANF3)

(define-syntax-class anf3-val
  #:conventions (anf3-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda formals M)))

(define-syntax-class anf3-binding-expr
  #:conventions (anf3-convention)
  #:literal-sets (kernel-literals)
  (pattern V)
  (pattern (#%plain-app x0 xs ...))
  (pattern (if x-test
               (#%plain-app x-true)
               (#%plain-app x-false))))

(define-syntax-class anf3-expr
  #:conventions (anf3-convention)
  #:literal-sets (kernel-literals)
  (pattern x)
  (pattern (letrec-values (((xs) B) ...) M)))

(define anf3? (syntax-class->predicate anf3-expr))

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
  (syntax-parse stx
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    #:literals (combining-letrec)
    [(#%expression e)
     (anf1-normalize #'e k)]

    [(#%plain-lambda formals u)
     #:with M (anf1-normalize #'u)
     (k #'(#%plain-lambda formals M))]

    [(let-or-letrec (((x0) u1)) u2)
     #:with M2 (anf1-normalize #'u2 k)
     (anf1-normalize #'u1 (pat-λ (r) #`(combining-letrec (((x0) r)) M2)))]

    [(let-or-letrec (((x0) u1) ((xs) us) ...) u2)
     (anf1-normalize #'(combining-letrec (((x0) u1))
                        (combining-letrec (((xs) us) ...)
                          u2))
                    k)]

    [(let-or-letrec () u1)
     (anf1-normalize #'u1 k)]

    [(if u1 u2 u3)
     #:with M2 (k (anf1-normalize #'u2))
     #:with M3 (k (anf1-normalize #'u3))
     (anf1-normalize-name #'u1 (pat-λ (t) #'(if t M2 M3)))]

    [(#%plain-app u us ...)
     (walk-with anf1-normalize-name
                #'(u us ...)
                (pat-λ (r) (k #'(#%plain-app . r))))]

    [V (k #'V)]))

(define (anf1-normalize-name stx k)
  (anf1-normalize
   stx
   (λ (u) (syntax-parse u
            #:conventions (anf1+2-convention)
            #:literal-sets (kernel-literals)
            #:literals (combining-letrec)
            [V (k u)]
            [_ #:with x (generate-temporary)
               #`(combining-letrec (((x) #,u))
                   #,(k #'x))]))))

;; ----------------------------------------

;; anf1->2: anf1? -> anf2?
(define (anf1->2 stx)
  (syntax-parse stx
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    #:literals (combining-letrec)
    [(combining-letrec (((x) V)) M)
     #:with S (anf1->2 #'M)
     #:with W (anf2-normalize-value #'V)
     #'(combining-letrec (((x) W)) S)]

    [(if V M1 M2)
     #:with S1 (anf1->2 #'M1)
     #:with S2 (anf1->2 #'M2)
     #:with (x-test x-true x-false x)
            (generate-temporaries #'(x-test x-true x-false x))
     #'(combining-letrec (((x-test) V))
         (combining-letrec (((x-true) (#%plain-lambda () S1)))
           (combining-letrec (((x-false) (#%plain-lambda () S2)))
             (combining-letrec (((x) (if x-test
                                         (#%plain-app x-true)
                                         (#%plain-app x-false))))
               x))))]

    [(combining-letrec (((x) (#%plain-app V Vs ...))) M)
     #:with S (anf1->2 #'M)
     (walk-with anf2-lift-value
                #'(V Vs ...)
                (pat-λ (r) #'(combining-letrec (((x) (#%plain-app . r)))
                               S)))]

    [(#%plain-app V Vs ...)
     #:with x (generate-temporary)
     (anf1->2
      #'(combining-letrec (((x) (#%plain-app V Vs ...)))
          x))]

    [x #'x]

    [V #:with x (generate-temporary)
       #:with W (anf2-normalize-value #'V)
       #'(combining-letrec (((x) W)) x)]))

(define (anf2-normalize-value v)
  (syntax-parse v
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    [(#%plain-lambda formals M)
     #:with S (anf1->2 #'M)
     #'(#%plain-lambda formals S)]

    [W #'W]))

(define (anf2-lift-value v k)
  (syntax-parse v
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    #:literals (combining-letrec)
    [x (k v)]
    [(#%plain-lambda formals M)
     #:with S (anf1->2 #'M)
     #:with t (generate-temporary)
     #`(combining-letrec (((t) (#%plain-lambda formals S)))
         #,(k #'t))]
    [(quote c)
     #:with t (generate-temporary)
     #`(combining-letrec (((t) (quote c)))
         #,(k #'t))]))

(define (anf2-normalize stx)
  (anf1->2 (anf1-normalize stx)))

;; ----------------------------------------

;; anf2->3 : anf2? -> anf3?
(define (anf2->3 stx)
  (syntax-parse stx
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    #:literals (combining-letrec)
    [x #'x]

    [(quote c) #'(quote c)]

    [(#%plain-lambda formals S)
     #:with u* (anf2->3 #'S)
     #'(#%plain-lambda formals u*)]

    [{~and e (#%plain-app x0 xs ...)}
     #'e]

    [{~and e (if x-test
                 (#%plain-app x-true)
                 (#%plain-app x-false))}
     #'e]

    [(combining-letrec (((xs) us) ...)
      (combining-letrec (((x0) u0))
        u1))
     (anf2->3
      #'(combining-letrec (((xs) us) ... ((x0) u0))
          u1))]

    [(combining-letrec (((xs) us) ...)
        x0)
     #:with (us* ...) (stx-map anf2->3 #'(us ...))
     #'(letrec-values (((xs) us*) ...)
         x0)]
    ;;
    ))

(define (anf3-normalize stx)
  (anf2->3 (anf2-normalize stx)))

;; ----------------------------------------

(module+ test
      (require racket/pretty)
  (define-namespace-anchor ns-anchor)
  (define ns (namespace-anchor->namespace ns-anchor))
  (parameterize ([current-namespace ns])
    (test-case "anf expand"
      (check-true  (anf1? #'(combining-letrec (((a) '1)) (#%plain-app + '1 '2))))
      (check-false (anf1? #'(let ((a (let ((b 1)) b))) a))))

    (test-case "anf normalize"
      (check-true
       (with-syntax ([x #'(if '#t (#%plain-app + '1 '2) (#%plain-app + '2 '3))]
                     [y #'(if '#f (#%plain-app + '3 '4) (#%plain-app / '1 '2))])
         (anf1? (anf1-normalize #'(#%plain-app + x y))))))

    (test-case "anf normalize 2"
      (define u1 (expand #'(#%plain-app + (let-values (((a) '1)) a) (let-values (((a) '2)) a))))
      (define M1 (anf1-normalize u1))
      (check-true (anf1? M1))
      (check-exn exn:fail:syntax? (λ () (eval-syntax M1) 3)))

    (test-case "anf fib"
      (define fib-stx
        (expand
         #'(let ((fib (lambda (n k)
                        (if (< n 2)
                            1
                            (+ (k (- n 1) k)
                               (k (- n 2) k))))))
             (fib 20 fib))))

      (check-false (anf1? fib-stx))

      (define fib-stx-anf1 (anf1-normalize fib-stx))
      (check-true (anf1? fib-stx-anf1))
      (check-false (anf2? fib-stx-anf1))

      (define fib-stx-anf2 (anf1->2 fib-stx-anf1))
      (check-true (anf2? fib-stx-anf2))

      (define fib-stx-anf3 (anf2->3 fib-stx-anf2))
      (check-true (anf3? fib-stx-anf3))

      (check-equal? 10946 (eval-syntax fib-stx-anf3)))

    (test-case "anf fib 2"
      (define fib-stx
        (expand
         #'(let ()
            (define (fib n)
              (if (< n 2)
                  1
                  (+ (fib (- n 1)) (fib (- n 2)))))
            (fib 20))))

      (define fib-stx-anf3 (anf3-normalize fib-stx))

      (check-true (anf3? fib-stx-anf3))
      (check-equal? 10946 (eval-syntax fib-stx-anf3))
    (pretty-print (syntax->datum fib-stx-anf3))
      )
    
    (test-case "anf let/let*/letrec"
      (check-equal?
       (eval-syntax
        (anf3-normalize
         (expand #'(let ([x 0])
                     (let ([x 1]
                           [y x])
                       (+ x y))))))
       1)

      (check-equal?
       (eval-syntax
        (anf3-normalize
         (expand #'(let ([x 0])
                     (let* ([x 1]
                            [y x])
                       (+ x y))))))
       2)

      (check-equal?
       (eval-syntax
        (anf3-normalize
         (expand #'(let ([x (thunk 0)]
                         [y 0])
                     (letrec ([x (thunk (+ y 1))]
                              [y 1])
                       (+ (x) y))))))
       3))
    ;
    ))
