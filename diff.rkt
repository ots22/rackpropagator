#lang racket/base

(require (for-syntax racket/base
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
         "primitives.rkt")

(module+ test
  (require rackunit
           syntax/macro-testing))

(define-for-syntax (prim-definition prim)
  (syntax-parse prim
    #:literals (+ - * / cons car cdr list identity apply make-list gen-zero)
    [+
     #'(λ xs
         (list (apply + xs)
               (λ (Aw) (cons '() (make-list (length xs) Aw)))))]

    ;; TODO fix signature
    [*
     #'(λ (x y)
         (list (* x y)
               (λ (Aw) (list '() (* Aw y) (* Aw x)))))]

    [cons
     #'(λ (a b)
         (list (cons a b)
               (λ (Aw) (list '() (car Aw) (cdr Aw)))))]

    [car
     #'(λ (xs)
         (list (car xs)
               (λ (Aw) (list '() (cons Aw (gen-zero))))))]
    [cdr
     #'(λ (xs)
         (list (cdr xs)
               (λ (Aw) (list '() (cons (gen-zero) Aw)))))]

    [list
     #'(λ xs
         (list xs
               (λ (Aw) (cons '() Aw))))]

    [identity
     #'(λ (x)
         (list x
               (λ (Aw) (list '() Aw))))]

    ;; TODO fix signature
    ;; [apply
    ;;  #'(λ (f args)
    ;;      (let ([result (apply f args)])
    ;;        (list (car result)
    ;;              (λ (Aw)
    ;;                (list '() ((cdr result) Aw))))))]

    [make-list
     #'(λ (n x)
         (list (make-list n x)
               (λ (Aw) (list '() 0 (apply + Aw)))))]



    [other #'(if (procedure? other)
                 (λ xs
                   (list
                    (apply other xs)
                    (λ (Aw)
                      (if (gen-zero? Aw)
                          (cons '() (make-list (length xs) Aw))
                          (raise-arguments-error 'prim-definition
                                                 "Backpropagator unknown"
                                                 "op" 'other)))))
                 other)]))

(define-syntax-rule (primal De)
  (let-values ([(p b) De]) p))

(define-syntax-rule (backprop De)
  (let-values ([(p b) De]) b))

(define-syntax (D+ stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
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
             (let ([primal+backprop (apply D+f xs)])
               (values (car primal+backprop)
                       (λ Aw
                         (coerce-zero
                          ;; drop terms from closed-over variables
                          (cdr (apply (cadr primal+backprop) Aw))
                          xs)))))))]))

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
                  [(y <-y) (D+f 184.0)])
     (check-equal? y 184.0)
     (check-equal? (<-y 1.0) '(1.0))))

  (test-case "conditional"
    (let*-values ([(D+f) (D+ (λ (a) (if (> a 10) a 0.0)))]
                  [(primal1 backprop1) (D+f 5.0)]
                  [(primal2 backprop2) (D+f 15.0)])
      (check-equal? primal1 0.0)
      (check-equal? primal2 15.0)
      (check-equal? (backprop1 1.0) '(0.0))
      (check-equal? (backprop2 1.0) '(1.0))))

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

  (test-case "pow"
    (define D+pow
      (D+ (λ (x n)
            (define (pow x n) (if (= n 0) 1.0 (* x (pow x (- n 1)))))
            (pow x n))))

    (check-equal? ((backprop (D+pow 2.0 3.0)) 1.0)
                  '(12.0 0.0)))

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

  ;;
  )


;; TODO

;; More examples - does anything break with this model?
;;   - Try example with mutually recursive functions

;; tidy up/refactor sum-destructuring-lazy-letrec and use of it

;; error messages (macros/syntax)

;; more backpropagators

;; example with repeated use of D+ (depends on fixing lambda formals)

;; trick for defining additional primitives nicely/extensibly
;; perhaps both:
;;   - register-backprop
;;   - define/backprop (use reverse-transform/backprop,
;;     then 'register' using the trick)

;; cosmetics for D+:
;;   - coerce gen-zero
;;   - drop closure variables
;;   - *explicit* closure variables can be passed by user

;; multiple values:
;;   - support in let-bindings and return from functions
;;   - use to return closure variables to avoid destructuring operations

;; set! (and functions that mutate)

;; quote-syntax
