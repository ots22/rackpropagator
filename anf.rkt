#lang racket/base

(require (for-template racket/base)
         racket/function
         racket/syntax
         syntax/parse
         "util.rkt")

(provide anf-convention
         anf-val
         anf-expr
         anf?
         simplified-anf-expr
         simplified-anf?
         anf-normalize
         simple-anf-normalize)

(module+ test
  (require rackunit))

;; ----------------------------------------
;; Syntax classes for ANF

(define-conventions anf-convention
  [x id]
  [x-test id]
  [x-true id]
  [x-false id]
  [x0 id]
  [xs id]

  [formals lambda-formals]

  [c anf-const]

  [#rx"^V" anf-val]
  [#rx"^M" anf-expr]

  [#rx"^B" simplified-anf-val]
  [#rx"^S" simplified-anf-expr])

(define-syntax-class lambda-formals
  (pattern (x:id ...)
           #:attr vars #'(x ...))
  (pattern xs:id
           #:attr vars #'(xs))
  (pattern (x:id ...+ . xs:id)
           #:attr vars #'(x ... xs)))

(define-syntax-class anf-const
  (pattern a:boolean)
  (pattern a:number))

(define-syntax-class anf-val
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda formals M)))

(define-syntax-class anf-expr
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern V0) ; return
  (pattern (#%plain-app V0 V ...)) ; tail call
  (pattern (let-values (((x) V0)) M)) ; bind
  (pattern (let-values (((x) (#%plain-app V0 V ...))) M)) ; call
  (pattern (if V0 M-true M-false))) ; branch

(define anf? (syntax-class->predicate anf-expr))

;; ----------------------------------------

;; 'Simplified' ANF
;;
;; Note that compared to ANF:
;;  - The value in 'return' position can be only a named variable
;;  - all function applications are mode in a let-values binding (no tail calls)
;;  - function applications involve named variables only

(define-syntax-class simplified-anf-val
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda formals S)))

(define-syntax-class simplified-anf-expr
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern x)
  (pattern (let-values (((x) B)) S))
  (pattern (let-values (((x) (#%plain-app x0 xs ...))) S))
  (pattern (let-values (((x) (if x-test
                                 (#%plain-app x-true)
                                 (#%plain-app x-false)))) S)))

(define simplified-anf? (syntax-class->predicate simplified-anf-expr))

;; ----------------------------------------

;; walk stx, which must be a syntax list, calling f on each element: f
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
(define (anf-normalize stx [k identity])
  (syntax-parse stx
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [(#%expression e)
     (anf-normalize #'e k)]

    [(#%plain-lambda formals u)
     #:with M (anf-normalize #'u)
     ;; compute free variables and add a syntax property (e.g. 'closure)
     (k #'(#%plain-lambda formals M))]

    [(let-values (((x) u1)) u2)
     #:with M2 (anf-normalize #'u2 k)
     (anf-normalize #'u1 (pat-λ (r) #'(let-values (((x) r)) M2)))]

    [(let-values (((x0) u1) ((xs) us) ...) u2)
     (anf-normalize #'(let-values (((x0) u1))
                        (let-values (((xs) us) ...)
                          u2))
                    k)]

    [(let-values () u1)
     (anf-normalize #'u1 k)]

    [(if u1 u2 u3)
     #:with M2 (k (anf-normalize #'u2))
     #:with M3 (k (anf-normalize #'u3))
     (anf-normalize-name #'u1 (pat-λ (t) #'(if t M2 M3)))]

    [(#%plain-app u us ...)
     (walk-with anf-normalize-name
                #'(u us ...)
                (pat-λ (r) (k #'(#%plain-app . r))))]

    [V (k #'V)]))

(define (anf-normalize-name stx k)
  (anf-normalize
   stx
   (λ (u) (syntax-parse u
            #:conventions (anf-convention)
            #:literal-sets (kernel-literals)
            [V (k u)]
            [_ #:with x (generate-temporary)
               #`(let-values (((x) #,u))
                   #,(k #'x))]))))

;; ----------------------------------------

;; simplify-anf: anf? -> simplified-anf?
(define (simplify-anf stx)
  (syntax-parse stx
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [(let-values (((x) V)) M)
     #:with S (simplify-anf #'M)
     #:with B (simplify-anf-value #'V)
     #'(let-values (((x) B)) S)]

    [(if V M1 M2)
     #:with S1 (simplify-anf #'M1)
     #:with S2 (simplify-anf #'M2)
     #:with (x-test x-true x-false x) (generate-temporaries
                                       #'(x-test x-true x-false x))
     #'(let-values (((x-test) V))
         (let-values (((x-true) (#%plain-lambda () S1)))
           (let-values (((x-false) (#%plain-lambda () S2)))
             (let-values (((x) (if x-test
                                   (#%plain-app x-true)
                                   (#%plain-app x-false))))
               x))))]

    [(let-values (((x) (#%plain-app V Vs ...))) M)
     #:with S (simplify-anf #'M)
     (walk-with simplify-anf-lift-value
                #'(V Vs ...)
                (pat-λ (r) #'(let-values (((x) (#%plain-app . r)))
                               S)))]

    [(#%plain-app V Vs ...)
     #:with x (generate-temporary)
     (simplify-anf
      #'(let-values (((x) (#%plain-app V Vs ...)))
          x))]

    [x #'x]

    [V #:with x (generate-temporary)
       #:with B (simplify-anf-value #'V)
       #'(let-values (((x) B)) x)]))

(define (simplify-anf-value v)
  (syntax-parse v
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [(#%plain-lambda formals M)
     #:with S (simplify-anf #'M)
     #'(#%plain-lambda formals S)]

    [B #'B]))

(define (simplify-anf-lift-value v k)
  (syntax-parse v
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [x (k v)]
    [(#%plain-lambda formals M)
     #:with S (simplify-anf #'M)
     #:with t (generate-temporary)
     #`(let-values (((t) (#%plain-lambda formals S)))
         #,(k #'t))]
    [(quote c)
     #:with t (generate-temporary)
     #`(let-values (((t) (quote c)))
         #,(k #'t))]))

(define (simple-anf-normalize stx)
  (simplify-anf (anf-normalize stx)))

;; ----------------------------------------

(module+ test
  (parameterize ([current-namespace (make-base-namespace)])
    (test-case "anf/expand"
      (check-true  (anf? (expand #'(let ((a 1)) (+ 1 2)))))
      (check-false (anf? (expand #'(let ((a (let ((b 1)) b))) a)))))

    (test-case "anf/normalize"
      (check-true
       (with-syntax ([x #'(if '#t (#%plain-app + '1 '2) (#%plain-app + '2 '3))]
                     [y #'(if '#f (#%plain-app + '3 '4) (#%plain-app / '1 '2))])
         (anf? (anf-normalize #'(#%plain-app + x y))))))

    (test-case "anf/normalize 2"
      (define u1 (expand #'(#%plain-app + (let-values (((a) '1)) a) (let-values (((a) '2)) a))))
      (define M1 (anf-normalize u1))
      (check-true (anf? M1))
      (check-equal? (eval M1) 3))

    (test-case "anf/normalize 3"
      (define fib-stx
        (expand
         #'(let ((fib (lambda (n k)
                        (if (< n 2)
                            1
                            (+ (k (- n 1) k)
                               (k (- n 2) k))))))
             (fib 20 fib))))

      (check-false (anf? fib-stx))

      (define fib-stx-anf (anf-normalize fib-stx))
      (check-true (anf? fib-stx-anf))
      (check-false (simplified-anf? fib-stx-anf))

      (define fib-stx-anf-simplified (simplify-anf fib-stx-anf))
      (check-true (simplified-anf? fib-stx-anf-simplified))

      (check-true (all-equal? 10946
                              (eval fib-stx)
                              (eval fib-stx-anf)
                              (eval fib-stx-anf-simplified))))))
