#lang racket/base

(require (for-template racket/base
                       "anf.rkt")
         (for-syntax racket/base
                     racket/list
                     racket/function
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     syntax/id-table
                     syntax/free-vars
                     "anf.rkt"
                     "reverse-transform.rkt"
                     "primitives.rkt"
                     "util.rkt")
         racket/list
         racket/function
         racket/unsafe/ops
         "anf.rkt"
         "primitives.rkt")

(module+ test
  (require racket/match
           syntax/macro-testing
           rackunit))

(define-for-syntax (prim-definition prim)
  (syntax-parse prim
    #:literals (+ - * / cons car cdr unsafe-car unsafe-cdr list list* identity
                apply make-list gen-zero)
    [+
     #'(λ xs
         (values (λ (Aw) (cons '() (make-list (length xs) Aw)))
                 (apply + xs)))]

    ;; TODO fix signature
    [*
     #'(λ (x y)
         (values (λ (Aw) (list '() (scale Aw y) (scale Aw x)))
                 (* x y)))]

    [cons
     #'(λ (a b)
         (values (λ (Aw) (list '() (zero-car Aw) (zero-cdr Aw)))
                 (cons a b)))]

    [car
     #'(λ (xs)
         (values (λ (Aw) (list '() (cons Aw (gen-zero))))
                 (car xs)))]
    [cdr
     #'(λ (xs)
         (values (λ (Aw) (list '() (cons (gen-zero) Aw)))
                 (cdr xs)))]

    [unsafe-car
     #'(λ (xs)
         (values (λ (Aw) (list '() (cons Aw (gen-zero))))
                 (unsafe-car xs)))]
    [unsafe-cdr
     #'(λ (xs)
         (values (λ (Aw) (list '() (cons (gen-zero) Aw)))
                 (unsafe-cdr xs)))]

    [list
     #'(λ xs
         (values (λ (Aw) (cons '() Aw))
                 xs))]

    [list*
     #'(λ xs
         (values (λ (Aw)
                   (cons '()
                         (call-with-values
                          (λ () (split-at Aw (sub1 (length xs))))
                          (λ (head tail) (append head (list tail))))))
                 (apply list* xs)))]

    [identity
     #'(λ (x)
         (values (λ (Aw) (list '() Aw))
                 x))]

    ;; TODO fix signature
    ;; [apply
    ;;  #'(λ (f args)
    ;;      (let ([result (apply f args)])
    ;;        (list (car result)
    ;;              (λ (Aw)
    ;;                (list '() ((cdr result) Aw))))))]

    [make-list
     #'(λ (n x)
         (values (λ (Aw) (list '() 0 (apply add Aw)))
                 (make-list n x)))]

    [other #'(if (procedure? other)
                 (λ xs
                   (apply values
                          (λ (Aw)
                            (if (gen-zero? Aw)
                                (cons '() (make-list (length xs) Aw))
                                (raise-arguments-error 'prim-definition
                                                       "Backpropagator unknown"
                                                       "op" 'other)))
                          (call-with-values (λ () (apply other xs))
                                            list)))
                 other)]))

(define-syntax-rule (primal De)
  (call-with-values (λ () De)
                    (λ vs (apply values (cdr vs)))))

(define-syntax-rule (backprop De)
  (call-with-values (λ () De)
                    (λ vs (car vs))))

(define-syntax (D+ stx)
  (syntax-parse stx
    #:literal-sets (anf-literals)
    [(_ e)
     ;;; Currently need to 'expand' here twice. The anf routines work
     ;;; with expanded programs, but also introduce identifiers, which
     ;;; might not be picked up by free-vars unless we expand again.
     #:do [(define simplified-e
             (anf-normalize (local-expand #'e 'expression '())))]
     #:with (_ (((_) e*)) _) (local-expand simplified-e 'expression '())
     #:with (De* (prim:id prim-intro:id) ...) (reverse-transform #'e*)
     #:with (prim-def ...) (stx-map prim-definition #'(prim ...))
     #'(let* ([prim-intro prim-def] ...)
         (let ([D+f De*])
           (λ xs
             (let-values ([(<-y ys) (call-with-values
                                     (λ () (apply D+f xs))
                                     (λ xs (values (car xs) (cdr xs))))])
               (apply values
                      (λ Aw
                        (coerce-zero
                         ;; drop terms from closed-over variables
                         (cdr (apply <-y Aw))
                         xs))
                      ys)))))]))

(module+ test
  (test-case "plus"
    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (D+ (λ (x) (+ x x))))))

    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (D+ (λ (x) (let ([f +])
                     (f x x))))))))

  (test-case "closure"
    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (let ((y 1))
          (D+ (lambda (x)
                (let ((result (+ x y)))
                  result))))))))

  (test-case "identity"
    (let*-values ([(D+f) (D+ (λ (a)
                               (((λ (b)
                                   (λ (c)
                                     b)) a) 1)))]
                  [(<-y y) (D+f 184.0)])
     (check-equal? y 184.0)
     (check-equal? (<-y 1.0) '(1.0))))

  (test-case "conditional"
    (let*-values ([(D+f) (D+ (λ (a) (if (> a 10) a 0.0)))]
                  [(<-y1 y1) (D+f 5.0)]
                  [(<-y2 y2) (D+f 15.0)])
      (check-equal? y1 0.0)
      (check-equal? y2 15.0)
      (check-equal? (<-y1 1.0) '(0.0))
      (check-equal? (<-y2 1.0) '(1.0))))

  (test-case "pow (Y)"
    (define D+pow
      (D+ (λ (x n)
            (let* ([pow* (λ (x n rec)
                           (if (= n 0)
                               1.0
                               (* x (rec x (- n 1) rec))))]
                   [pow (λ (x n) (pow* x n pow*))])
              (pow x n)))))
    (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
                  '(12.0 0.0)))

  ;; These fail: No letrec yet
  
  ;; (test-case "pow"
  ;;   (define D+pow
  ;;     (D+ (λ (x n)
  ;;           (define (pow x n) (if (= n 0) 1.0 (* x (pow x (- n 1)))))
  ;;           (pow x n))))

  ;;   (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
  ;;                 '(12.0 0.0)))

  ;; (test-case "pow 2"
  ;;   (define D+pow
  ;;     (D+ (λ (x n)
  ;;           (define (pow x n r) (if (= n 0) r (pow x (- n 1) (* r x))))
  ;;           (pow x n 1))))

  ;;   (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
  ;;                 '(12.0 0.0)))

  ;; (test-case "scale gen-zero"
  ;;   (define D+pow
  ;;     (D+ (λ (x)
  ;;           ;; f accumulates a result into r, but do not use it. Check
  ;;           ;; that the backpropagator can handle scaling by the
  ;;           ;; resulting gen-zero sensitivity.
  ;;           (define (f x r) (if (< x 0) 1 (f (- x 1) (* r x))))
  ;;           (f x 1))))

  ;;   (check-equal? ((backprop (D+pow 2.0)) 1.0)
  ;;                 '(0.0)))

  ;; (test-case "Mutual recursion"
  ;;   (define D+fn
  ;;     (D+
  ;;      (λ (x n)
  ;;        (letrec ([f (λ (x n) (if (= n 0)
  ;;                                 x
  ;;                                 (* x (g x (- n 1)))))]
  ;;                 [g (λ (x n) (if (= n 0)
  ;;                                 x
  ;;                                 (+ x (f x (- n 1)))))])
  ;;          (f x n)))))

  ;;   (let-values ([(y <-y) (D+fn 5 5)])
  ;;     (check-equal? y 775)
  ;;     (check-equal? (<-y 1.0) '(585.0 0.0))))

  (test-case "Different final use"
    (check-equal?
     ((backprop ((D+ (λ (x) (let ([b 1]) x))) 2)) 1)
     '(1))

    (let ([D+f (let ([a 1]) (D+ (λ (x) (let ([b a]) x))))])
      (check-equal?
       ((backprop (D+f 2)) 1)
       '(1))))

  (test-case "list/cons/car/cdr"
    (check-equal?
     ((backprop ((D+ (λ (x y) (list (car x) (cdr x) y x y)))
             '(1 2) 3))
      '(1 1 0 (0 . 0) 0))
     '((1 . 1) 0)))

  (test-case "lambda formals"
    (check-equal?
     ((backprop ((D+ (λ xs (car xs))) 2.0 3.0)) 1.0)
     '(1.0 0.0))

    (check-equal?
     ((backprop ((D+ (λ (x . xs) (car xs))) 2.0 3.0 4.0)) 1.0)
     '(0.0 1.0 0.0))
    )

  (test-case "Primitive binding"
    (define * +)
    (define-values (<-y y) ((D+ (λ (x) (* x x))) 10))
    (check-equal? y 20)
    ;; TODO: fix this
    (check-exn
     exn:fail?
     (λ () (<-y 1))))

  (test-case "match-let"
    (define Df (D+ (λ (x) (match-let ([(list a b) x]) (+ a b)))))
    (define-values (<-y y) (Df '(1 2)))
    (check-equal? y 3)
    (check-equal? (<-y 1) '((1 1))))

  (test-case "second derivative"
    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (D+ (λ (y) ((backprop ((D+ (λ (x) x)) y)) 1))))))

    (check-not-exn
     (λ ()
       (convert-compile-time-error
        (D+ (λ (y) ((backprop ((D+ (λ (x) (+ x x))) y)) 1)))))))

  (test-case "Multiple values"
    (check-equal? 
     (call-with-values (λ () (primal ((D+ (λ (y) (values 1 2))) 1)))
                       list)
     '(1 2)))
  ;;
  )


;; TODO

;; tidy up/refactor sum-destructuring-lazy-letrec and use of it

;; error messages (macros/syntax)

;; more backpropagators

;; example with repeated use of D+

;; trick for defining additional primitives nicely/extensibly
;; perhaps both:
;;   - register-backprop
;;   - define/backprop (use reverse-transform/backprop,
;;     then 'register' using the trick)

;; cosmetics for D+:
;;   - explicit closure variables can be passed by user

;; multiple values:
;;   - support in let-bindings and return from functions
;;   - use to return closure variables to avoid destructuring operations

;; begin/begin0 as (let () ...) (?); multiple let body forms

;; set! (and functions that mutate)
;;   - global table of adjoints (of values - set-box! rather than set!)
;;   - uses of 'box' create something in the adjoint table (representing
;;     the internal state - or something like that)
;;   - convert uses of set! into set-box!
