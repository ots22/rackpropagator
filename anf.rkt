#lang racket/base

(require (for-template racket/base)
         (for-syntax racket/base)
         racket/function
         racket/syntax
         syntax/stx
         syntax/parse
         "util.rkt")

(provide anf-convention
         (rename-out [anf2-val anf-val]
                     [anf2-binding-expr anf-binding-expr]
                     [anf2-expr anf-expr]
                     [anf2? anf?]
                     [anf2-normalize anf-normalize]))

(module+ test
  (require rackunit))

;; ----------------------------------------
;; Conventions and syntax classes

(define-syntax-class lambda-formals
  (pattern (x:id ...)
           #:attr vars #'(x ...))
  (pattern xs:id
           #:attr vars #'(xs))
  (pattern (x:id ...+ . xs:id)
           #:attr vars #'(x ... xs)))

(define-syntax-class anf-const
  (pattern a:boolean)
  (pattern a:number)
  (pattern ()))

(define-conventions anf1+2-convention
  [#rx"^x" id]
  [formals lambda-formals]

  [c anf-const]

  [#rx"^V" anf1-val]
  [#rx"^M" anf1-expr]

  [#rx"^W" anf2-val]
  [#rx"^B" anf2-binding-expr]
  [#rx"^S" anf2-expr])

(define-conventions anf-convention
  [#rx"^x" id]
  [formals lambda-formals]

  [c anf-const]

  [#rx"^V" anf2-val]
  [#rx"^B" anf2-binding-expr]
  [#rx"^M" anf2-expr])

;; ----------------------------------------
;; A-normal form, first kind (ANF1)

(define-syntax-class anf1-val
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote e))
  (pattern (quote-syntax e {~optional #:local}))
  (pattern x)
  (pattern (#%plain-lambda formals M)))

(define-syntax-class anf1-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern V0) ; return
  (pattern (#%plain-app V0 V ...)) ; tail call
  (pattern (let-values (((x) V0)) M)) ; bind
  (pattern (let-values (((x) (#%plain-app V0 V ...))) M)) ; call
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
  (pattern (quote e))
  (pattern (quote-syntax e {~optional #:local}))
  (pattern x)
  (pattern (#%plain-lambda formals S)))

(define-syntax-class anf2-binding-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern W)
  (pattern (#%plain-app x0 xs ...))
  (pattern (if x-test
               (#%plain-app x-true)
               (#%plain-app x-false))))

(define-syntax-class anf2-expr
  #:conventions (anf1+2-convention)
  #:literal-sets (kernel-literals)
  (pattern x)
  (pattern (let-values (((x) B)) S)))
  
(define anf2? (syntax-class->predicate anf2-expr))

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
    [(#%expression e)
     (anf1-normalize #'e k)]

    [(#%plain-lambda formals u)
     #:with M (anf1-normalize #'u)
     (k #'(#%plain-lambda formals M))]

    [(let-values (((x0) u1)) u2)
     #:with M2 (anf1-normalize #'u2 k)
     (anf1-normalize #'u1 (pat-λ (r) #`(let-values (((x0) r)) M2)))]

    [(let-values (((x0) u1) ((xs) us) ...) u2)
     (anf1-normalize #'(let-values (((x0) u1))
                        (let-values (((xs) us) ...)
                          u2))
                    k)]

    [(let-values () u1)
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
            [V (k u)]
            [_ #:with x (generate-temporary)
               #`(let-values (((x) #,u))
                   #,(k #'x))]))))

;; ----------------------------------------

;; anf1->2: anf1? -> anf2?
(define (anf1->2 stx)
  (syntax-parse stx
    #:conventions (anf1+2-convention)
    #:literal-sets (kernel-literals)
    [(let-values (((x) V)) M)
     #:with S (anf1->2 #'M)
     #:with W (anf2-normalize-value #'V)
     #'(let-values (((x) W)) S)]

    [(if V M1 M2)
     #:with S1 (anf1->2 #'M1)
     #:with S2 (anf1->2 #'M2)
     #:with (x-test x-true x-false x)
            (generate-temporaries #'(x-test x-true x-false x))
     #'(let-values (((x-test) V))
         (let-values (((x-true) (#%plain-lambda () S1)))
           (let-values (((x-false) (#%plain-lambda () S2)))
             (let-values (((x) (if x-test
                                         (#%plain-app x-true)
                                         (#%plain-app x-false))))
               x))))]

    [(let-values (((x) (#%plain-app V Vs ...))) M)
     #:with S (anf1->2 #'M)
     (walk-with anf2-lift-value
                #'(V Vs ...)
                (pat-λ (r) #'(let-values (((x) (#%plain-app . r)))
                               S)))]

    [(#%plain-app V Vs ...)
     #:with x (generate-temporary)
     (anf1->2
      #'(let-values (((x) (#%plain-app V Vs ...)))
          x))]

    [x #'x]

    [V #:with x (generate-temporary)
       #:with W (anf2-normalize-value #'V)
       #'(let-values (((x) W)) x)]))

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
    [x (k v)]
    [(#%plain-lambda formals M)
     #:with S (anf1->2 #'M)
     #:with t (generate-temporary)
     #`(let-values (((t) (#%plain-lambda formals S)))
         #,(k #'t))]
    [(quote e)
     #:with t (generate-temporary)
     #:with v* v
     #`(let-values (((t) v*))
         #,(k #'t))]
    [(quote-syntax e {~optional #:local})
     #:with t (generate-temporary)
     #:with v* v
     #`(let-values (((t) v*))
         #,(k #'t))]
    ))

(define (anf2-normalize stx)
  (anf1->2 (anf1-normalize stx)))

;; ----------------------------------------

(module+ test
  (define-namespace-anchor ns-anchor)
  (define ns (namespace-anchor->namespace ns-anchor))
  (parameterize ([current-namespace ns])
    (test-case "anf expand"
      (check-true  (anf1? #'(let-values (((a) '1)) (#%plain-app + '1 '2))))
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
      (check-equal? (eval-syntax M1) 3))

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

      (check-equal? 10946 (eval-syntax fib-stx-anf2)))

    ;; TODO: letrec
    ;; (test-case "anf fib 2"
    ;;   (define fib-stx
    ;;     (expand
    ;;      #'(let ()
    ;;         (define (fib n)
    ;;           (if (< n 2)
    ;;               1
    ;;               (+ (fib (- n 1)) (fib (- n 2)))))
    ;;         (fib 20))))

    ;;   (define fib-stx-anf2 (anf2-normalize fib-stx))

    ;;   (check-true (anf2? fib-stx-anf2))
    ;;   (check-equal? 10946 (eval-syntax fib-stx-anf2)))
    
    (test-case "anf let/let*/letrec"
      (check-equal?
       (eval-syntax
        (anf2-normalize
         (expand #'(let ([x 0])
                     (let ([x 1]
                           [y x])
                       (+ x y))))))
       1)

      (check-equal?
       (eval-syntax
        (anf2-normalize
         (expand #'(let ([x 0])
                     (let* ([x 1]
                            [y x])
                       (+ x y))))))
       2)

      ;; TODO: letrec
      ;; (check-equal?
      ;;  (eval-syntax
      ;;   (anf2-normalize
      ;;    (expand #'(let ([x (thunk 0)]
      ;;                    [y 0])
      ;;                (letrec ([x (thunk (+ y 1))]
      ;;                         [y 1])
      ;;                  (+ (x) y))))))
      ;;  3)
      )
    ;
    ))
